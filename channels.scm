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

(define (instantiate-origins origins)
  "Instantiate ORIGINS and return their location in the store."
  (with-store store
    (run-with-store store
      (mlet* %store-monad
          ((drvs (mapm/accumulate-builds origin->derivation origins))
           (_    (built-derivations drvs)))
        (return (map derivation->output-path drvs))))))

;;; XXX: Adapted from (guix transformations).
(define (patched-source name source maildirs)
  "Return a file-like object with the given NAME that applies MAILDIRS to
SOURCE.  SOURCE must itself be a file-like object of any type, including
<git-checkout>, <local-file>, etc."
  (define quilt
    (module-ref (resolve-interface '(gnu packages patchutils)) 'quilt))
  (define gawk
    (module-ref (resolve-interface '(gnu packages gawk)) 'gawk))

  (computed-file name
                 (with-imported-modules '((guix build utils))
                   #~(begin
                       (use-modules (guix build utils)
                                    (srfi srfi-34))
                       (setenv "PATH"
                               (string-append #+gawk "/bin:"
                                              #+quilt "/bin:"
                                              (getenv "PATH")))

                       (copy-recursively #+source #$output)
                       (chdir #$output)
                       (for-each
                        (lambda (maildir)
                          (setenv "QUILT_PATCHES" maildir)
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
                        '(#+@maildirs))))))

(define maybe-instantiate-channel
  (match-lambda
    ((? channel? ch)
     ch)
    (((? channel? ch) . (? list? patches))
     (if (file-exists? (channel-url ch))
         ch
         (checkout->channel-instance
          (with-store store
            (run-with-store store
              (mlet* %store-monad
                  ((drv (lower-object
                         (patched-source
                          (symbol->string (channel-name ch))
                          (git-checkout
                           (url (channel-url ch))
                           (branch (channel-branch ch))
                           (commit (channel-commit ch)))
                          (map instantiate-origin patches))))
                   (_ (built-derivations (list drv))))
                (return (derivation->output-path drv)))))
          #:commit (channel-commit ch)
          #:url (channel-url ch)
          #:name (channel-name ch))))))


(define %channels
  (let ((cwd (dirname (current-filename))))
    (list
     (cons
      (channel
       (name 'guix)
       (branch "master")
       (commit (and (not (file-exists? (string-append cwd "/channels/guix")))
                    "c5fa9dd0e96493307cc76ea098a6bca9b076e012"))
       (introduction
        (make-channel-introduction
         "9edb3f66fd807b096b48283debdcddccfea34bad"
         (openpgp-fingerprint
          "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))
       (url
        (if (file-exists? (string-append cwd "/channels/guix"))
            (string-append cwd "/channels/guix")
            "https://git.savannah.gnu.org/git/guix.git")))
      (list
       (origin
         (method patchset-fetch)
         (uri (patchset-reference
               (type 'gnu) (id 65613) (version 1)))
         (sha256
          (base32
           "05vwh940ak8yv01r2gxfr1ikwk4pi4kl6wxpdm4si8ri7j4kman4")))))

     (cons
      (channel
       (name 'nonguix)
       (url
        (if (file-exists? (string-append cwd "/channels/nonguix"))
            (string-append cwd "/channels/nonguix")
            "https://gitlab.com/nonguix/nonguix.git"))
       (branch "master")
       (commit (and (not (file-exists? (string-append cwd "/channels/nonguix")))
                    "6e864249c2025863e18e42587cb42764a99bec27"))
       (introduction
        (make-channel-introduction
         "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
         (openpgp-fingerprint
          "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      '())

     (cons
      (channel
       (name 'rde)
       (branch "master")
       (commit (and (not (file-exists? (string-append cwd "/channels/rde")))
                    "74a3fb8378e86603bb0f70b260cbf46286693392"))
       (introduction
        (make-channel-introduction
         "257cebd587b66e4d865b3537a9a88cccd7107c95"
         (openpgp-fingerprint
          "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0")))
       (url
        (if (file-exists? (string-append cwd "/channels/rde"))
            (string-append cwd "/channels/rde")
            "https://git.sr.ht/~abcdw/rde")
        ))
      (list
       ;; (origin  ; rde: mail: Allow unset emacs-ednc and gpg-primary-key values
       ;;  (method patchset-fetch)
       ;;  (uri (patchset-reference
       ;;        (type 'srht) (project "~abcdw/rde-devel") (id 54111) (version 1)))
       ;;  (sha256
       ;;   (base32 "1xjg8kc5i6sbcjnd9s1djl1dx9kg92il43afizg72si5pp0hfs9l")))
       (origin  ; age password-store
         (method patchset-fetch)
         (uri (patchset-reference
               (type 'srht) (project "~abcdw/rde-devel") (id 36511) (version 2)))
         (sha256
          (base32 "1xjg8kc5i6sbcjnd9s1djl1dx9kg92il43afizg72si5pp0hfs9l")))
       (origin  ; Guix's SSH configuration
         (method patchset-fetch)
         (uri (patchset-reference
               (type 'srht) (project "~abcdw/rde-devel") (id 40004) (version 3)))
         (sha256
          (base32 "0d103n0vwwqc8l5mlj7sjzw402ris7qhrz6fvr95qwvhhn0i1v1a")))
       (origin  ; SSH option ssh-add-keys
         (method patchset-fetch)
         (uri (patchset-reference
               (type 'srht) (project "~abcdw/rde-devel") (id 40007) (version 1)))
         (sha256
          (base32 "1khdmm392v19mp1710rbk2wfm4zibnpi9knx0yh0si603f0bj1bz")))
       (origin  ; org-roam-todo unecessary sync
         (method patchset-fetch)
         (uri (patchset-reference
               (type 'srht) (project "~abcdw/rde-devel") (id 44345) (version 1)))
         (sha256
          (base32 "1390wpb2ng8x866i5yswyf3mhl6gzfscqfq82wn30c8vn9kmgk1h")))
       (origin  ; org-roam-file-exclude-regexp
         (method patchset-fetch)
         (uri (patchset-reference
               (type 'srht) (project "~abcdw/rde-devel") (id 39539) (version 4)))
         (sha256
          (base32 "0vckbkwh3x07p4b57pj1h6bldbsayl2cbysrc00pybl8vml7sh61")))
       (origin  ; sway focus emacs-client frames.
         (method patchset-fetch)
         (uri (patchset-reference
               (type 'srht) (project "~abcdw/rde-devel") (id 47806) (version 1)))
         (sha256
          (base32 "0n09agca480mcfirwgl23bmpjpc02xkm5bc82mn6bnjs9zq6kvkb")))))
     (cons
      (channel
       (name 'odf-dsfr)
       (url
        (if (file-exists? (string-append cwd "/channels/odf-dsfr"))
            (string-append cwd "/channels/odf-dsfr")
            "https://git.sr.ht/~codegouvfr/odf-dsfr"))
       ;; (branch "master")
       (commit "af1b66927f2dc968549a978626150b5f2c1afd37"))
      '()))))

(map maybe-instantiate-channel %channels)
