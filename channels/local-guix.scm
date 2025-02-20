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
              (delete-file-recursively "out"))
            ;; Not upstreamble, this is to avoid searching in those.
            (call-with-output-file ".rgignore"
              (lambda (port)
                (format port "out")))))
        ;; The source is the current working directory.
        (delete 'unpack)
        (add-before 'build 'flag-as-cached
          (lambda _
            (call-with-output-file "guix-configured.stamp"
              (const #t)))))))

(define* (is-guix-up-to-date? guix-directory
                              #:key (make (which "make")))
  "Compute if Guix is up-to-date in the sense of GNU make.
This enables us not to try and run build steps when not necessary."
  (with-directory-excursion guix-directory
    (catch #t
      (lambda ()
        (and
         ;; First check the two SUBDIRS of guix.
         (invoke make "-q" "po/guix")
         (invoke make "-q" "po/packages")
         ;; Thanks to the phase 'stamp-make-go-steps, make-go is also
         ;; a timestamped file, checking it allows us to ensure go files
         ;; are built.
         ;; FIXME This is not working as intended.
         (invoke make "SUBDIRS=" "-q" "make-go")
         (every
          (cut invoke make "SUBDIRS=" "-q" <>)
          ;; We had (blame) some guile code to calculate these files,
          ;; but it takes a lot of time on a section of code we want
          ;; to run often that should barely never change. The bash
          ;; equivalent to find them is:
          ;; make -pn | grep '^all:' | tail -1 | sed 's/all: //' | tr ' ' '\n'
          '("doc/os-config-bare-bones.texi"
            "doc/os-config-desktop.texi"
            "doc/os-config-lightweight-desktop.texi"
            "doc/he-config-bare-bones.scm"
            "nix/libstore/schema.sql.hh"
            ".version"))))
      (lambda args
        #f))))

(define* (get-local-guix #:key (path (string-append (getcwd) "/guix")))
  (with-store store
    (let* ((repo (repository-open path))
           (commit (oid->string
                    (object-id (revparse-single repo "master"))))
           (version (git-version "1.4.0" "0" commit)))
      (and
       (or
        (is-guix-up-to-date? path)
        (build-in-local-container
         store
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
               (local-phases
                #~(modify-phases #$phases
                    ;; Disable translations for speed.
                    (add-before 'bootstrap 'disable-translations
                      (lambda _
                        (substitute* "bootstrap"
                          (("for lang in \\$\\{langs\\}")
                           "for lang in "))
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
                    ;; Stamp make-go steps to improve caching
                    ;; FIXME This is not working as intended.
                    (add-before 'bootstrap 'stamp-make-go-steps
                      (lambda _
                        (substitute* "Makefile.am"
                          (("\\$\\$\\(filter %\\.scm,\\$\\$\\^\\)" all)
                           (string-append all " ;\t\\\n\t\ttouch $(1)"))
                          (((string-append
                             "^\\.PHONY: make-("
                             (string-join
                              (append '("core" "system" "cli")
                                      (map (compose
                                            (cut string-append "packages" <>)
                                            number->string)
                                           (iota 6)))
                              "|")
                             ")-go"))
                           "")
                          (("^\\.PHONY: make-packages-go")
                           "\t@touch $@")
                          (("^\\.PHONY: clean-go make-go as-derivation")
                           ".PHONY: clean-go as-derivation")
                          (("^make-go:.*" all)
                           (string-append all "\t@touch $@\n")))))
                    ;; FIXME arguments substitutions other than phases
                    ;; don't seem to apply : tests are run despite #:tests? #f
                    (delete 'copy-bootstrap-guile)
                    (delete 'set-SHELL)
                    (delete 'check)
                    ;; FIXME strip has the same issue
                    ;; => Run it in copy-build-system for now.
                    (delete 'strip)
                    ;; Run it only when we need to debug, saves us a few seconds.
                    (delete 'validate-runpath))
                ;; phases-ignored-when-configured
                '(disable-failing-tests
                  disable-translations
                  stamp-make-go-steps
                  bootstrap
                  patch-usr-bin-file
                  patch-source-shebangs
                  configure
                  patch-generated-file-shebangs
                  use-host-compressors)
                path)))))))
       (package/inherit guix
         (version version)
         (source
          (local-file "guix/out" "local-guix" #:recursive? #t))
         (build-system copy-build-system)
         (arguments
          (list #:substitutable? #f
                #:strip-directories #~'("libexec" "bin")
                #:validate-runpath? #f
                #:phases
                #~(modify-phases %standard-phases
                    ;; The next phases have been applied already.
                    ;; No need to repeat them several times.
                    (delete 'validate-documentation-location)
                    (delete 'delete-info-dir-file)))))))))

(define local-guix (get-local-guix))

(define* (is-channel-up-to-date? path
                                 #:optional (source-directory ".")
                                 #:key (effective "3.0"))
  (with-directory-excursion path
    (let* ((out-dir (string-append "out/lib/guile/" effective "/site-ccache"))
           (scm-files (if (equal? source-directory ".")
                          (find-files
                           source-directory
                           (lambda (file stat)
                             (and (not (string-prefix? "./out" file))
                                  (string-suffix? ".scm" file))))
                          (find-files source-directory "\\.scm$")))
           (prefix-length (string-length source-directory)))

      (define (go-file-path scm-file)
        (let ((file-sans-extension (string-drop
                                    (string-drop-right scm-file 4)
                                    prefix-length)))
          (string-append out-dir file-sans-extension ".go")))

      (define (needs-recompilation? scm-file)
        (let* ((go-file (go-file-path scm-file)))
          (or (not (file-exists? go-file))
              (> (stat:mtime (stat scm-file))
                 (stat:mtime (stat go-file))))))

      (and (directory-exists? "out")
           (not (any needs-recompilation? scm-files))))))

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
            (guile guile-3.0)
            (effective "3.0")
            (pkg
             (package
               (name name)
               (version (string-take commit 7))
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
                 (list #:phases
                       (local-phases #~%standard-phases
                                     phases-ignored-when-configured
                                     path))))
               (inputs (append (list guile local-guix)
                               (map make-channel-package dependencies)))
               (home-page home-page)
               (synopsis (string-append name " channel"))
               (description (string-append name " channel"))
               (license license:gpl3+))))
       (with-store store
         (and (or (is-channel-up-to-date? path
                                          (if (equal? src-directory "/")
                                              "."
                                              (string-drop src-directory 1))
                                          #:effective effective)
                  (build-in-local-container store pkg))))
       (package/inherit pkg
         (source
          (local-file (string-append path "/out")
                      (string-append "local-" name)
                      #:recursive? #t))
         (build-system copy-build-system)
         (arguments
          (list #:substitutable? #f
                #:validate-runpath? #f
                #:phases
                #~(modify-phases %standard-phases
                    ;; The next phases have been applied already.
                    ;; No need to repeat them several times.
                    (delete 'validate-documentation-location)
                    (delete 'delete-info-dir-file)))))))))

(directory-union
 "guix-with-channels"
 (cons* local-guix
        (map make-channel-package
             ;; This supposes subdirectories are channels.
             (filter (lambda (file)
                       (and (eq? (stat:type (lstat file)) 'directory)
                            (not (member file '("." ".." "guix")))))
                     (scandir ".")))))
