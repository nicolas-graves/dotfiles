;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2021-2024 Nicolas Graves <ngraves@ngraves.fr>

(use-modules
 (guix packages)
 ((guix self) #:select (make-config.scm))
 (guix modules)
 (guix monads)
 (guix derivations)
 (gnu packages guile)
 (gnu packages tls)
 (gnu packages version-control)
 (guix channels)
 (guix store)
 (guix scripts)
 (guix gexp)
 (guix git)
 (guix records)
 (srfi srfi-26)
 (srfi srfi-1)
 (ice-9 match)
 (guix build utils))

;; Pre-definitions. These are actually defined in (guix-stack patchset)
;; and will be overwritten by the (use-modules) if available.
;; This allows us to have a functioning file even in a profile sans guix-stack.
;; I can't move these definitions inside the catch for some reason.
(define make-patched-channel car)
(define patchset-fetch identity)
(define-record-type* <patchset-reference>
  patchset-reference make-patchset-reference
  patchset-reference?
  (type patchset-reference-type)
  (id patchset-reference-id)
  ;; A version, when possible, is higly recommended to enhance reproducibility
  (version patchset-reference-version
           (default 0))
  ;; here project encompasses repositories (github, gitlab), mailing lists (srht)
  (project patchset-reference-project
           (default #f)))

(catch #t
  (lambda ()
    (use-modules (guix-stack patchset)))
  (lambda (key . args)
    (display "Module (guix-stack patchset) not found. Falling back...\n")))

(define-record-type* <patched-channel>
  patched-channel make-patched-channel
  patched-channel?
  (channel patched-channel-channel)  ; <channel>
  (patchsets patched-channel-patchsets  ; list of <origin>
             (default '())))

(define* (patchset-fetch ref hash-algo hash #:optional name
                     #:key (system %current-system) guile)

  (define uri
    (apply
     format
     #f
     (assoc-ref
      '((gnu . "https://debbugs.gnu.org/cgi-bin/bugreport.cgi?bug=~a;mbox=yes")
        (srht . "https://lists.sr.ht/~a/patches/~a/mbox")
        ;; Forges currently don't work, refer to https://github.com/mricon/b4/issues/25
        (github . "https://patch-diff.githubusercontent.com/raw/~a/pull/~a.patch")
        (gitlab . "https://gitlab.com/~a/-/merge_requests/~a.patch")
        (codeberg . "https://codeberg.org/~a/pulls/~a.patch"))
      (patchset-reference-type ref))
     (append (or (and=> (patchset-reference-project ref) list) '())
             (list (patchset-reference-id ref)))))

  (define modules
    (cons `((guix config) => ,(make-config.scm))
          (delete '(guix config)
                  (source-module-closure '((guix build download)
                                           (guix build utils))))))

  (define build
    (with-extensions (list guile-json-4 guile-gnutls)
      (with-imported-modules modules
        #~(begin
            (use-modules (guix build utils) (guix build download))
            (setenv "TMPDIR" (getcwd))
            (setenv "XDG_DATA_HOME" (getcwd))
            (invoke #$(file-append b4 "/bin/b4")
                    "-d" "-n" "--offline-mode" "--no-stdin"
                    "am" "--no-cover" "--no-cache"
                    "--use-local-mbox"
                    (url-fetch #$uri "mbox" #:verify-certificate? #f)
                    #$@(if (eq? 0 (patchset-reference-version ref))
                           '()
                           (list "--use-version"
                                 (number->string
                                  (patchset-reference-version ref))))
                    "--no-add-trailers"
                    "--outdir" "."
                    "--quilt-ready")
            (copy-recursively
             (car (find-files "." "\\.patches" #:directories? #t))
             #$output)))));)

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system)))
    (gexp->derivation (or name
                          (match-record ref <patchset-reference>
                                        (type id version)
                            (format #f "~a-~a-v~a-patchset" type id version)))
      build
      ;; Use environment variables and a fixed script name so
      ;; there's only one script in store for all the
      ;; downloads.
      #:system system
      #:local-build? #t ;don't offload repo cloning
      #:hash-algo hash-algo
      #:hash hash
      #:recursive? #t
      #:guile-for-build guile)))

;;; XXX: Adapted from (guix transformations).
(define (patched-source name source patches-or-patchsets)
  "Return a file-like object with the given NAME that applies MAILDIRS to
SOURCE.  SOURCE must itself be a file-like object of any type, including
<git-checkout>, <local-file>, etc."
  (define gawk
    (module-ref (resolve-interface '(gnu packages gawk)) 'gawk))
  (define patch
    (module-ref (resolve-interface '(gnu packages base)) 'patch))
  (define quilt
    (module-ref (resolve-interface '(gnu packages patchutils)) 'quilt))

  (computed-file name
                 (with-imported-modules '((guix build utils))
                   #~(begin
                       (use-modules (guix build utils)
                                    (srfi srfi-34)
                                    (ice-9 match))
                       (define (quilt-patchset? candidate)
                          (and (directory-exists? candidate)
                               (file-exists? (string-append candidate "/series"))))
                       (define (quilt-push!)
                            (with-exception-handler
                                 (lambda (exception)
                                    (and (invoke-error? exception)
                                        ;; 2 is not an error.
                                        (not (= 2 (invoke-error-exit-status
                                                   exception)))
                                        (report-invoke-error exception)))
                               (lambda ()
                                 (invoke "quilt" "push" "-afv" "--leave-rejects"))
                               #:unwind? #t))
                       (setenv "PATH"
                               (string-append #+patch "/bin:"
                                              #+gawk "/bin:"
                                              #+quilt "/bin:"
                                              (getenv "PATH")))

                       (copy-recursively #+source #$output)
                       (chdir #$output)
                       (for-each
                        (match-lambda 
                          ((? quilt-patchset? maildir)
                             (setenv "QUILT_PATCHES" maildir)
                             (quilt-push!))
                          (patch
                            (invoke "patch" "-p1" "--batch" "-i" patch)))
                        '(#+@patches-or-patchsets))))))

(define (patched-channel->channel-instance patched-channel)
  (match-record patched-channel <patched-channel>
                (channel patchsets)
    ((@@ (guix channels) channel-instance)
     channel
     (channel-commit channel)
     (patched-source
      (symbol->string (channel-name channel))
      (git-checkout
       (url (channel-url channel))
       (branch (channel-branch channel))
       (commit (channel-commit channel)))
      patchsets))))

(define maybe-instantiate-channel
  (match-lambda
    ((? channel? channel)
     channel)
    ((? patched-channel? patched-channel)
     (let ((channel (patched-channel-channel patched-channel)))
       (if (file-exists? (channel-url channel))
           channel
           (patched-channel->channel-instance patched-channel))))))

(define %channels
  (let* ((cwd (dirname (current-filename)))
         (submodule (cut string-append cwd "/channels/" <>))
         (submodule? (compose file-exists? submodule))
         ;; (submodule? (negate submodule?))
         )
    (list
     (make-patched-channel
      (channel
       (name 'guix)
       (branch "master")
       (commit (and (not (submodule? "guix"))
                    "2dc38e493beaabb3f8d8c8b646a9374efc17db67"))
       (introduction
        (make-channel-introduction
         "9edb3f66fd807b096b48283debdcddccfea34bad"
         (openpgp-fingerprint
          "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))
       (url
        (if (submodule? "guix")
            (submodule "guix")
            "https://git.savannah.gnu.org/git/guix.git")))
      (list
       (origin
         (method patchset-fetch)
         (uri (patchset-reference
               (type 'gnu) (id 65613) (version 1)))
         (sha256
          (base32
           "1sk64pysrfvalj94b05g96gdxkfksdv3rh4q35bzm2syz2rm5pgl")))
       (origin ;; emacs-persid
         (method patchset-fetch)
         (uri (patchset-reference
               (type 'gnu) (id 73534) (version 1)))
         (sha256
          (base32
           "1pq92cna75qysvn207i770mk7w5h8gi5dsp0k92vxybhgyi32288")))
       (origin ;; guix shell allow/revoke
         (method patchset-fetch)
         (uri (patchset-reference
               (type 'gnu) (id 73166) (version 1)))
         (sha256
          (base32
           "11hw1abma3hvbbhqhzhmr66xbyj0rv7plxjys5n5vgg7rrs6v3r9")))
       (origin ;; Decoupling gtk@4 from qtbase@5.
         (method patchset-fetch)
         (uri (patchset-reference
               (type 'gnu) (id 74517) (version 3)))
         (sha256
          (base32
           "1563vrkibmysk2fm4sgyw68s5cp189v0vcbwgc75jzq2mpprynxp")))
       (origin ;; Decoupling pipewire from qtbase.
         (method patchset-fetch)
         (uri (patchset-reference
               (type 'gnu) (id 74589) (version 2)))
         (sha256
          (base32
           "02z4b0v8qj1p1rb98yq809m0851v0hd8i9gghq1vjgk1pvwbsk81")))
       (local-file "patches/guix-channels-Enable-file-like-channel-instance-checkout.patch")))

     (make-patched-channel
      (channel
       (name 'nonguix)
       (url
        (if (submodule? "nonguix")
            (submodule "nonguix")
            "https://gitlab.com/nonguix/nonguix.git"))
       (branch "master")
       (commit (and (not (submodule? "nonguix"))
                    "6e864249c2025863e18e42587cb42764a99bec27"))
       (introduction
        (make-channel-introduction
         "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
         (openpgp-fingerprint
          "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      '())

     (make-patched-channel
      (channel
       (name 'rde)
       (branch "master")
       (commit (and (not (submodule? "rde"))
                    "bc3d6ea1fef988c0d8c1bd5bf0ab0ae83c148251"))
       (introduction
        (make-channel-introduction
         "257cebd587b66e4d865b3537a9a88cccd7107c95"
         (openpgp-fingerprint
          "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0")))
       (url
        (if (submodule? "rde")
            (submodule "rde")
            "https://git.sr.ht/~abcdw/rde")))

      (list
       (origin  ; age password-store
         (method patchset-fetch)
         (uri (patchset-reference
               (type 'srht) (project "~abcdw/rde-devel") (id 36511) (version 2)))
         (sha256
          (base32 "0rbf59jj7rvqg4k305ppf4g6j137pzd9079qfg4vhhrib0w81296")))
       (origin  ; SSH option ssh-add-keys
         (method patchset-fetch)
         (uri (patchset-reference
               (type 'srht) (project "~abcdw/rde-devel") (id 40007) (version 1)))
         (sha256
          (base32 "14blms5ck7pgi4c41m99bqkxpg16xsbnf9x940dxipc2h28hfkfw")))
       (origin  ; org-roam-todo unecessary sync
         (method patchset-fetch)
         (uri (patchset-reference
               (type 'srht) (project "~abcdw/rde-devel") (id 44345) (version 1)))
         (sha256
          (base32 "13a21xzj1sgiljm4ffks4dxlmknv58983yg7528zax84dnp5x6vr")))
       (origin  ; org-roam-file-exclude-regexp
         (method patchset-fetch)
         (uri (patchset-reference
               (type 'srht) (project "~abcdw/rde-devel") (id 39539) (version 4)))
         (sha256
          (base32 "0c8rf0hwij0rf5grwz596ncyi4l0mainpfkf03mcds6slihd11v6")))
       (origin  ; sway focus emacs-client frames.
         (method patchset-fetch)
         (uri (patchset-reference
               (type 'srht) (project "~abcdw/rde-devel") (id 47806) (version 1)))
         (sha256
          (base32 "0fz93dkgsss70dlc1xcgdc3jzjylq2wys0k547h2635dk7322a7z")))
       (origin  ;; Wrap pass-binary to handle multiline files.
         (method patchset-fetch)
         (uri (patchset-reference
               (type 'srht) (project "~abcdw/rde-devel") (id 53611) (version 2)))
         (sha256
          (base32 "1xmb838s64h5p4gdhcrqcqszrcbjdmxrxh06akzr8rgdjn9s35ad")))
       (local-file "patches/rde-project-Disable-broken-configuration.patch")))
     (make-patched-channel
      (channel
       (name 'odf-dsfr)
       (branch "master")
       (commit (and (not (submodule? "odf-dsfr"))
                    "af1b66927f2dc968549a978626150b5f2c1afd37"))
       (url
        (if (submodule? "odf-dsfr")
            (submodule "odf-dsfr")
            "https://git.sr.ht/~codegouvfr/odf-dsfr")))
      '())
     (make-patched-channel
      (channel
       (name 'guix-rde)
       (branch "master")
       (commit (and (not (submodule? "guix-rde"))
                    "97a32354e796324937da35fb6d430fde382fb2fe"))
       (url
        (if (submodule? "guix-rde")
            (submodule "guix-rde")
            "https://git.sr.ht/~ngraves/guix-rde")))
      '())
     (make-patched-channel
      (channel
       (name 'guix-science)
       (branch "master")
       (commit (and (not (submodule? "guix-science"))
                    "be44985a2d468ed8bcc09ab4bf320a4e3b6c09be"))
       (url
        (if (submodule? "guix-science")
            (submodule "guix-science")
            "https://codeberg.org/guix-science/guix-science")))
      '())
     (make-patched-channel
      (channel
       (name 'guix-science-nonfree)
       (branch "master")
       (commit (and (not (submodule? "guix-science-nonfree"))
                    "5b8c3f38ee81dd090ca5fdc531eecde248c37c86"))
       (url
        (if (submodule? "guix-science-nonfree")
            (submodule "guix-science-nonfree")
            "https://codeberg.org/guix-science/guix-science-nonfree")))
      '())
     (make-patched-channel
      (channel
       (name 'guix-past)
       (branch "master")
       (commit (and (not (submodule? "guix-past"))
                    "2d3485b7fd7c1904bc7c1a87fc45048376ff4d3a"))
       (url
        (if (submodule? "guix-past")
            (submodule "guix-past")
            "https://codeberg.org/guix-science/guix-past")))
      '())
     (make-patched-channel
      (channel
       (name 'guix-stack)
       (branch "master")
       (commit (and (not (submodule? "guix-stack"))
                    "67c456cf24e654234ff9e8642d6cf4ac916801fc"))
       (url
        (if (submodule? "guix-stack")
            (submodule "guix-stack")
            "https://git.sr.ht/~ngraves/guix-stack")))
      '())
     (make-patched-channel
      (channel
       (name 'nrepl-python)
       (branch "master")
       (commit (and (not (submodule? "nrepl-python"))
                    "67c456cf24e654234ff9e8642d6cf4ac916801fc"))
       (url
        (if (submodule? "nrepl-python")
            (submodule "nrepl-python")
            "https://git.sr.ht/~ngraves/nrepl-python")))
      '()))))

(map maybe-instantiate-channel %channels)
