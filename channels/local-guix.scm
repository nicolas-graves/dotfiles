(use-modules (git)
             (guix git)
             (guix git-download)
             (guix gexp)
             (guix channels)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (guix profiles)
             (guix store)
             ((guix utils) #:select (substitute-keyword-arguments))
             (guix memoization)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-71)
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
             (guix build guile-build-system)
             (guix-stack build local-build-system))

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

(define* (is-guix-up-to-date? guix-directory
                              #:key (make (which "make")))
  "Compute if Guix is up-to-date in the sense of GNU make.
This enables us not to try and run build steps when not necessary."
  (with-directory-excursion guix-directory
    (catch #t
      (lambda ()
        (and
         (file-exists? "guix-configured.stamp")
         ;; First check the two SUBDIRS of guix.
         (invoke make "-q" "po/guix")
         (invoke make "-q" "po/packages")
         ;; Check that compiled files are up-to-date.
         (is-channel-up-to-date?
          guix-directory
          #:exclude-find-files-pred
          (lambda (file stat)
            (and (string-suffix? ".scm" file)
                 (not (string-prefix? "./out" file))
                 (not (string-prefix? "./tests" file))
                 (not (string-prefix? "./build-aux" file))
                 (not (string-prefix? "./gnu/installer" file))
                 (not (string-prefix? "./gnu/tests" file))
                 (not (string-prefix? "./doc" file))
                 (not (string-prefix? "./etc" file))
                 (not (member file '("./gnu/build/locale.scm"
                                     "./gnu/build/po.scm"
                                     "./gnu/build/shepherd.scm"
                                     "./gnu/build/svg.scm"
                                     "./guix/build/po.scm"
                                     "./guix/man-db.scm"
                                     "./guix/scripts/system/installer.scm"
                                     "./manifest.scm"
                                     "./meta-test.scm"
                                     "./test.scm"))))))
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
                          #:target-directory path
                          ;; FIXME Unclear why srfi-26 can only be used at top-level.
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

(define* (is-channel-up-to-date? path
                                 #:optional (source-directory ".")
                                 #:key (effective "3.0")
                                 (exclude-find-files-pred
                                  (if (equal? source-directory ".")
                                      (lambda (file stat)
                                        (and (not (string-prefix? "./out" file))
                                             (string-suffix? ".scm" file)))
                                      "\\.scm$")))
  (with-directory-excursion path
    (let* ((out-dir (string-append "out/lib/guile/" effective "/site-ccache"))
           (scm-files (find-files source-directory exclude-find-files-pred))
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

(define make-channel-package+instance
  (memoize
   (lambda (path)
     (let* ((dir (dirname path))
            (name (basename path))
            (repo (repository-open path))
            (commit-ref
             (oid->string
              (object-id (catch 'git-error
                           (lambda () (revparse-single repo "master"))
                           (lambda _ (revparse-single repo "main"))))))
            (origin (remote-lookup repo "origin"))
            (uri (remote-url origin))
            (home-page (if (string-prefix? "git@" uri)
                           (error
                            (format
                             #f "~a: origin remote is not an http public link"
                             path))
                           uri))
            (local-channel (channel
                            (name (string->symbol name))
                            ;; Currently all are using master.
                            (branch "master")
                            (commit commit-ref)
                            (url home-page))))
       (match name
         ("guix" (values (get-local-guix)
                         ((@@ (guix channels) channel-instance)
                          local-channel commit-ref path)))
         (_
          (let* ((metadata
                  ((@@ (guix channels) read-channel-metadata-from-source) path))
                 (src-directory
                  ((@@ (guix channels) channel-metadata-directory) metadata))
                 (dependencies
                  (map (compose (cut string-append dir "/" <>)
                                symbol->string
                                channel-name)
                       ((@@ (guix channels) channel-metadata-dependencies)
                        metadata)))
                 (phases-ignored-when-configured
                  '(patch-usr-bin-file
                    patch-source-shebangs
                    patch-generated-file-shebangs))
                 (guile guile-3.0)
                 (effective "3.0")
                 (pkg
                  (package
                    (name name)
                    (version (string-take commit-ref 7))
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
                    (inputs
                     (let ((guix-pkg instance (make-channel-package+instance
                                               (string-append dir "/guix"))))
                       (append
                        (list guile guix-pkg)
                        (map make-channel-package+instance dependencies))))
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
            (values
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
                          (delete 'delete-info-dir-file)))))
             ((@@ (guix channels) channel-instance)
              local-channel commit-ref path)))))))))

(define* (local-channels->manifest names #:key (target-directory (getcwd)))

  (define (local-channel->entry instance pkg)
    (let* ((channel (channel-instance-channel instance))
           (commit  (channel-instance-commit instance)))
      (manifest-entry
        (name (symbol->string (channel-name channel)))
        (version (string-take commit 7))
        (item pkg)
        (properties
         `((source ,(channel-instance->sexp instance)))))))

  (manifest (map (lambda (name)
                   (let* ((path (string-append target-directory "/" name))
                          (pkg instance (make-channel-package+instance path)))
                     (local-channel->entry instance pkg)))
                 names)))

(with-store store
  (run-with-store store
    (profile-derivation
     (local-channels->manifest
      (filter (lambda (file)
                (and (eq? (stat:type (lstat file)) 'directory)
                     (not (member file '("." ".." "guix-science-nonfree")))))
              (scandir ".")))
     #:hooks %channel-profile-hooks
     #:format-version 3)))
