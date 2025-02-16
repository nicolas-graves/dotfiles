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
             (guix build-system guile))

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
         old-lower target-directory imported-modules)
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
              (with-imported-modules imported-modules
                #~(begin
                    (use-modules #$@imported-modules)
                    (with-directory-excursion #$target-directory
                      (for-each
                       (lambda (out)
                         (setenv
                          out (string-append #$target-directory "/" out)))
                       '#$outputs)
                      #$builder)))))))))))

(define* (make-local-build-system target-build-system
                                  #:key
                                  (target-directory (getcwd))
                                  (imported-modules '((guix build utils))))
  (build-system
    (name (symbol-append
           (build-system-name target-build-system) '-local))
    (description (string-append
                  (build-system-description target-build-system)
                  " ; applied as current user in " target-directory))
    (lower (make-local-lower (build-system-lower target-build-system)
                             target-directory
                             imported-modules))))

(define* (local-guix #:key (path (string-append (getcwd) "/guix")))
  (with-store store
    (let* ((repo (repository-open path))
           (commit (oid->string
                    (object-id (revparse-single repo "master"))))
           (version (git-version "1.4.0" "0" commit))
           (phases-ignored-when-cached
            '(;; separate-from-pid1
              ;; set-SOURCE-DATE-EPOCH
              ;; set-paths
              ;; install-locale
              ;; unpack  ; Ignored in both cases.
              disable-failing-tests
              bootstrap
              patch-usr-bin-file
              patch-source-shebangs
              configure
              patch-generated-file-shebangs
              use-host-compressors
              ;; set-font-path
              ;; build
              ;; copy-bootstrap-guile
              ;; set-SHELL
              ;; check
              ;; install
              ;; wrap-program
              ;; strip
              ;; validate-runpath
              ;; validate-documentation-location
              ;; delete-info-dir-file
              ;; patch-dot-desktop-files
              ;; make-dynamic-linker-cache
              ;; install-license-files
              ;; reset-gzip-timestamps
              ;; compress-documentation
              ))
           (pkg
            (package/inherit guix
              (version version)
              (source #f)
              (build-system (make-local-build-system
                             (package-build-system guix)
                             ;; FIXME Unclear why srfi-26 can only be used at top-level.
                             #:target-directory path
                             #:imported-modules '((guix build utils) (srfi srfi-26))))
              (arguments
               (substitute-keyword-arguments (package-arguments guix)
                 ((#:phases phases)
                  (let ((filtered-phases
                         (if (file-exists? (string-append path "/guix.cached"))
                             ;; This fold is a simple opposite filter-alist based on key.
                             #~(begin
                                 (use-modules (srfi srfi-1))
                                 (fold
                                  (lambda (key result)
                                    (if (member (car key) '#$phases-ignored-when-cached)
                                        result
                                        (cons key result)))
                                  '()
                                  (reverse #$phases)))
                             phases)))
                    #~(modify-phases #$filtered-phases
                        ;; The source is the current working directory.
                        (delete 'unpack)
                        ;; FIXME arguments substitutions other than phases
                        ;; don't seem to apply : tests are run despite #:tests? #f
                        (delete 'copy-bootstrap-guile)
                        (delete 'set-SHELL)
                        (delete 'check)
                        ;; FIXME strip has the same issue
                        ;; => Run it in copy-build-system for now.
                        (delete 'strip)
                        ;; Run it only when we need to debug, saves us a few seconds.
                        (delete 'validate-runpath)
                        (add-before 'install-locale 'delete-former-output
                          (lambda _
                            (when (file-exists? "out")
                              (delete-file-recursively "out"))))
                        (add-before 'build 'flag-as-cached
                          (lambda _
                            (call-with-output-file "guix.cached"
                              (const #t)))))))))))
           ;; We can't use package->derivation directly because we want the
           ;; user rather than the daemon to build the derivation.
           ;; This allows us to have access to the pre-built files without
           ;; having to mess with hashes or timestamps.
           (manifest (package->development-manifest guix))
           (bag (package->bag pkg))
           ;; See (@@ (guix scripts environment) manifest->derivation).
           (prof-drv ((store-lower profile-derivation)
                      store manifest #:allow-collisions? #t))
           (drv ((@@ (guix packages) bag->derivation*) store bag pkg))
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
            (('quit 0)
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
                          (delete 'delete-info-dir-file))))))
            (_ #f)))))))

(define make-channel-package
  (memoize
   (lambda (name)
     (let* ((repo (repository-open name))
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
                           metadata)))))
       (package
         (name name)
         (version (git-version "0.0.0" "0" commit))
         (source (let ((top (string-append (getcwd) "/" name)))
                   (local-file top
                               name
                               #:recursive? #t
                               #:select? (git-predicate top))))
         (build-system guile-build-system)
         (arguments
          (if (equal? src-directory "/")
              '()
              (list #:source-directory (string-drop src-directory 1))))
         (inputs (append (list guile-3.0 (local-guix))
                         (map make-channel-package dependencies)))
         (home-page home-page)
         (synopsis (string-append name " channel"))
         (description (string-append name " channel"))
         (license license:gpl3+))))))

(directory-union
 "guix-with-channels"
 (cons* local-guix
        (map make-channel-package
             ;; This supposes subdirectories are channels.
             (filter (lambda (file)
                       (and (eq? (stat:type (lstat file)) 'directory)
                            (not (member file '("." ".." "guix")))))
                     (scandir ".")))))
