(use-modules (git)
             (guix git)
             (guix git-download)
             (guix gexp)
             (guix channels)
             (guix derivations)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (guix profiles)
             (guix store)
             (guix utils)
             (guix memoization)
             (guix monads)
             (guix scripts environment)
             (gnu system file-systems)
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 ftw)
             (ice-9 match)
             (gnu packages)
             (gnu packages guile)
             (gnu packages package-management)
             (guix build utils)
             (guix build-system)
             (guix build-system copy)
             (guix build-system gnu)
             (guix build-system guile)
             (guix build guile-build-system))

;; GNU Guix is phenomenal in terms of extensibility and software
;; reproducibility. Some recent blog articles summed up how to use
;; Guix for local package development, see:
;; https://guix.gnu.org/blog/2023/from-development-environments-to\
;; -continuous-integrationthe-ultimate-guide-to-software-development-with-guix
;; One drawback of local development with Guix is the inability to
;; reuse compiled binary files for rapid software development: Guix
;; systematically rebuilds the whole package using all build phases.
;; This makes developping / hacking on heavy packages quite tedious.
;; In the absence of a better alternative, this hack/script allows to
;; develop locally by creating an equivalent store output from a local
;; repository using build phases from Guix source.

;; This requires a guix with patch series 68315 v3 applied to be run.

;; Important : We need to go through the store and derivations, since
;; we want to get the phases from Guix source. However, the derivation
;; builder can only affect the store. Thus the code needs to be
;; executed by the user. (I've also tried wide directory permissions,
;; which aren't enough. Maybe there's a way to build this using the
;; build daemon with the --disable-chroot option. But starting a new
;; daemon for this seems overkill).

;; We separate phases that are only needed to be applied once and phases
;; that need to be repeated each time the source is modified.

(define (make-local-lower
         old-lower target-directory modules)
  (lambda* args
    (let ((old-bag (apply old-lower args)))
      (bag
        (inherit old-bag)
        (build
         (lambda* (name inputs #:key (outputs '("out"))
                        #:allow-other-keys #:rest rest)
           (mlet %store-monad
               ((builder (apply (bag-build old-bag)
                                name inputs #:outputs outputs rest)))
             (return
              #~(begin
                  (use-modules #$@modules)
                  (with-directory-excursion #$target-directory
                    (for-each
                     (lambda (out)
                       (setenv
                        out (string-append #$target-directory "/" out)))
                     '#$outputs)
                    #$builder))))))))))

(define* (make-local-build-system target-build-system
                                  #:key
                                  (target-directory (getcwd))
                                  (modules '((guix build utils))))
  (build-system
    (name (symbol-append
           (build-system-name target-build-system) '-local))
    (description (string-append
                  (build-system-description target-build-system)
                  " ; applied as current user in " target-directory))
    (lower (make-local-lower (build-system-lower target-build-system)
                             target-directory modules))))

(define* (build-in-local-container store package)
  "Build local PACKAGE in a container locally."
  (with-store store
    ;; We can't use package->derivation directly because we want the
    ;; user rather than the daemon to build the derivation.
    ;; This allows us to have access to the pre-built files without
    ;; having to mess with hashes or timestamps.
    (let* ((manifest (package->development-manifest package))
           (bag (package->bag package))
           ;; See (@@ (guix scripts environment) manifest->derivation).
           (prof-drv ((store-lower profile-derivation)
                      store manifest #:allow-collisions? #t))
           (drv ((@@ (guix packages) bag->derivation*) store bag package))
           (_ (build-derivations store
                                 (cons* prof-drv (derivation-inputs drv))))
           (profile (derivation->output-path prof-drv)))
      (catch #t
        (lambda ()
          ((store-lower launch-environment/container)
           store
           #:command (cons* (derivation-builder drv)
                            (derivation-builder-arguments drv))
           #:bash (string-append profile "/bin/bash")
           #:map-cwd? #t
           #:user-mappings
           (list (specification->file-system-mapping "/gnu/store" #f))
           #:profile profile
           #:manifest manifest))
        (lambda args
          (match args
            (('quit 0) #t)
            (_         #f)))))))

(define (local-phases phases to-ignore path)
  "Modify phases to incorporate configured phases caching logic."
  (let ((filtered-phases
         (if (file-exists?
              (string-append path "/guix-configured.stamp"))
             ;; This fold is a simple opposite filter-alist based on key.
             #~(begin
                 (use-modules (srfi srfi-1))
                 (fold
                  (lambda (key result)
                    (if (member (car key) '#$to-ignore)
                        result
                        (cons key result)))
                  '()
                  (reverse #$phases)))
             phases)))
    #~(modify-phases #$filtered-phases
        (add-before 'unpack 'delete-former-output
          (lambda _
            (when (file-exists? "out")
              (delete-file-recursively "out"))))
        ;; The source is the current working directory.
        (delete 'unpack)
        (add-before 'build 'flag-as-cached
          (lambda _
            (call-with-output-file "guix-configured.stamp"
              (const #t)))))))

(define* (get-local-guix #:key (path (string-append (getcwd) "/guix")))
  (with-store store
    (let* ((repo (repository-open path))
           (commit (oid->string
                    (object-id (revparse-single repo "master"))))
           (version (git-version "1.4.0" "0" commit))
           (phases-ignored-when-configured
            '(disable-failing-tests
              disable-translations
              bootstrap
              patch-usr-bin-file
              patch-source-shebangs
              configure
              patch-generated-file-shebangs
              use-host-compressors))
           (pkg
            (package/inherit guix
              (version version)
              (source #f)
              (build-system (make-local-build-system
                             (package-build-system guix)
                             ;; FIXME Unclear why srfi-26 can only be used at top-level.
                             #:target-directory path
                             #:modules '((guix build utils)
                                         (srfi srfi-1)
                                         (srfi srfi-26))))
              (arguments
               (substitute-keyword-arguments (package-arguments guix)
                 ;; Disable translations for speed.
                 ((#:configure-flags flags #~'())
                  #~(cons* "--disable-nls" #$flags))
                 ((#:phases phases #~%standard-phases)
                  #~(modify-phases
                        #$(local-phases
                           phases phases-ignored-when-configured path)
                      ;; Disable translations for speed.
                      (add-before 'bootstrap 'disable-translations
                        (lambda _
                          (substitute* "bootstrap"
                            (("for lang in \\$\\{langs\\}")
                             "for lang in {}"))
                          (substitute* "Makefile.am"
                            (("include po/doc/local\\.mk")
                             "EXTRA_DIST ="))
                          (substitute* "doc/local.mk"
                            (("^(MANUAL|COOKBOOK)_LANGUAGES = .*" all type)
                             (string-append type "_LANGUAGES =\n"))
                            ;; This is the rule following info_TEXINFOS.
                            (("%C%_guix_TEXINFOS =" all)
                             (string-append
                              "info_TEXINFOS=%D%/guix.texi %D%/guix-cookbook.texi\n"
                              all)))))
                      ;; FIXME arguments substitutions other than phases
                      ;; don't seem to apply : tests are run despite #:tests? #f
                      (delete 'copy-bootstrap-guile)
                      (delete 'set-SHELL)
                      (delete 'check)
                      ;; FIXME strip has the same issue
                      ;; => Run it in copy-build-system for now.
                      (delete 'strip)
                      ;; Run it only when we need to debug, saves us a few seconds.
                      (delete 'validate-runpath))))))))
      (and (build-in-local-container store pkg)
           (package/inherit guix
             (version version)
             (source
              (local-file "guix/out" "local-guix"
                          #:recursive? #t
                          #:select? (const #t)))
             (build-system copy-build-system)
             (arguments
              (list #:strip-directories #~'("libexec" "bin")
                    #:validate-runpath? #f
                    #:phases
                    #~(modify-phases %standard-phases
                        ;; The next phases have been applied already.
                        ;; No need to repeat them several times.
                        (delete 'validate-documentation-location)
                        (delete 'delete-info-dir-file)))))))))

(define local-guix (get-local-guix))

;; This should work in theory, but since we're not able currently
;; to check if the guix package is up-to-date (regarding the need to
;; run post-build steps), each time we're rebuilding Guix, we'll need
;; to rebuild all channels... rendering this not that useful.
;; TODO Maybe there's an easy way to transform make-go filter-rules in
;; a proper use of stamp files that will allow that in Guix upstream.
(define make-channel-package
  (memoize
   (lambda (name)
     (let* ((path (string-append (getcwd) "/" name))
            (repo (repository-open name))
            (commit (oid->string
                     (object-id (catch 'git-error
                                  (lambda () (revparse-single repo "master"))
                                  (lambda _ (revparse-single repo "main"))))))
            (origin (remote-lookup repo "origin"))
            (uri (remote-url origin))
            (home-page (if (string-prefix? "git@git.sr.ht:" uri)
                           (string-append
                            "https://git.sr.ht/"
                            (string-drop
                             uri (string-length "git@git.sr.ht:")))
                           uri))
            (metadata
             ((@@ (guix channels) read-channel-metadata-from-source) name))
            (src-directory
             ((@@ (guix channels) channel-metadata-directory) metadata))
            (dependencies
             (remove (cut equal? <> "guix")
                     (map (compose symbol->string channel-name)
                          ((@@ (guix channels) channel-metadata-dependencies)
                           metadata))))
            (phases-ignored-when-configured
             '(patch-usr-bin-file
               patch-source-shebangs
               patch-generated-file-shebangs))
            (pkg
             (package
               (name name)
               (version (git-version "0.0.0" "0" commit))
               (source #f)
               (build-system (make-local-build-system
                              guile-build-system
                              #:target-directory path
                              #:modules '((guix build utils)
                                          (ice-9 match)
                                          (srfi srfi-1))))
               (arguments
                (append
                 (if (equal? src-directory "/")
                     '()
                     (list #:source-directory (string-drop src-directory 1)))
                 (list
                  #:modules '((guix build guile-build-system)
                              (guix build utils)
                              (ice-9 match)
                              (srfi srfi-1))
                  #:phases
                  #~(modify-phases
                        #$(local-phases
                           #~%standard-phases phases-ignored-when-configured path)
                      (replace 'build
                        (lambda* (#:key outputs inputs native-inputs
                                  (source-directory ".")
                                  (compile-flags '())
                                  ;; FIXME: Turn on parallel building of Guile modules by
                                  ;; default after the non-determinism issues in the Guile byte
                                  ;; compiler are resolved (see bug #20272).
                                  (parallel-build? #f)
                                  (scheme-file-regexp %scheme-file-regexp)
                                  (not-compiled-file-regexp #f)
                                  target
                                  #:allow-other-keys)
                          "Build files in SOURCE-DIRECTORY that match SCHEME-FILE-REGEXP.  Files
matching NOT-COMPILED-FILE-REGEXP, if true, are not compiled but are
installed; this is useful for files that are meant to be included."
                          (let* ((out        (assoc-ref outputs "out"))
                                 (guile      (assoc-ref (or native-inputs inputs) "guile"))
                                 (effective  (target-guile-effective-version guile))
                                 (module-dir (string-append out "/share/guile/site/"
                                                            effective))
                                 (go-dir     (string-append out "/lib/guile/"
                                                            effective "/site-ccache/"))
                                 (guild      (string-append guile "/bin/guild"))
                                 (flags      (if target
                                                 (cons (string-append "--target=" target)
                                                       compile-flags)
                                                 compile-flags)))
                            (if target
                                (format #t "Cross-compiling for '~a' with Guile ~a...~%"
                                        target effective)
                                (format #t "Compiling with Guile ~a...~%" effective))
                            (format #t "compile flags: ~s~%" flags)

                            ;; Make installation directories.
                            (mkdir-p module-dir)
                            (mkdir-p go-dir)

                            ;; Compile .scm files and install.
                            (setenv "GUILE_AUTO_COMPILE" "0")
                            (setenv "GUILE_LOAD_COMPILED_PATH"
                                    (string-append go-dir
                                                   (match (getenv "GUILE_LOAD_COMPILED_PATH")
                                                     (#f "")
                                                     (path (string-append ":" path)))))

                            (let ((source-files
                                   (with-directory-excursion source-directory
                                     (find-files "." scheme-file-regexp))))
                              (for-each
                               (lambda (file)
                                 (install-file (string-append source-directory "/" file)
                                               (string-append module-dir
                                                              "/" (dirname file))))
                               source-files)
                              ((@@ (guix build guile-build-system) invoke-each)
                               (filter-map
                                (lambda (file)
                                  (and (or (not not-compiled-file-regexp)
                                           (not (string-match not-compiled-file-regexp
                                                              file)))
                                       (cons* guild
                                              "guild" "compile"
                                              "-L" source-directory
                                              "-o" (string-append
                                                    go-dir
                                                    ((@@ (guix build guile-build-system)
                                                         file-sans-extension)
                                                     file)
                                                    ".go")
                                              (string-append source-directory "/" file)
                                              flags)))
                                source-files)
                               #:max-processes (if parallel-build? (parallel-job-count) 1)
                               #:report-progress
                               (@@ (guix build guile-build-system) report-build-progress))))))))))
               (inputs (append (list guile-3.0 local-guix)
                               (map make-channel-package dependencies)))
               (home-page home-page)
               (synopsis (string-append name " channel"))
               (description (string-append name " channel"))
               (license license:gpl3+))))
       (with-store store
         (and (build-in-local-container store pkg)
              (package/inherit pkg
                (source
                 (local-file (string-append name "/out")
                             (string-append "local-" name)
                             #:recursive? #t
                             #:select? (const #t)))
                (build-system copy-build-system)
                (arguments
                 (list #:validate-runpath? #f
                       #:phases
                       #~(modify-phases %standard-phases
                           ;; The next phases have been applied already.
                           ;; No need to repeat them several times.
                           (delete 'validate-documentation-location)
                           (delete 'delete-info-dir-file)))))))))))

(directory-union
 "guix-with-channels"
 (cons* local-guix
        (map make-channel-package
             ;; This supposes subdirectories are channels.
             (filter (lambda (file)
                       (and (eq? (stat:type (lstat file)) 'directory)
                            (not (member file '("." ".." "guix")))))
                     (scandir ".")))))
