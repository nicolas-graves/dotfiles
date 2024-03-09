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
  (version patchset-reference-version))

(define* (patchset-fetch ref hash-algo hash #:optional name
                     #:key (system %current-system) guile)

  (define uri
    (format
     #f
     (assoc-ref
      '((gnu . "https://debbugs.gnu.org/cgi-bin/bugreport.cgi?bug=~a;mbox=yes")
        (rde . "https://lists.sr.ht/~~abcdw/rde-devel/patches/~a/mbox"))
       (patchset-reference-type ref))
     (patchset-reference-id ref)))

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
                    "--use-version"
                    (number->string #$(patchset-reference-version ref))
                    "--no-add-trailers"
                    "--outdir" "."
                    "--quilt-ready")
            (for-each (lambda (file) (install-file file #$output))
                      (find-files
                       (car (find-files "." "\\.patches" #:directories? #t))
                       "\\.patch"))))));)

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

;; XXX: Copied from guix/packages.scm.
(define instantiate-patch
    (match-lambda
      ((? string? patch)                          ;deprecated
       (local-file patch #:recursive? #t))
      ((? struct? patch)                          ;origin, local-file, etc.
       patch)))

;;; XXX: Copied from (guix transformations).
(define (patched-source name source patches)
  "Return a file-like object with the given NAME that applies PATCHES to
SOURCE.  SOURCE must itself be a file-like object of any type, including
<git-checkout>, <local-file>, etc."
  (define patch
    (module-ref (resolve-interface '(gnu packages base)) 'patch))

  (computed-file name
                 (with-imported-modules '((guix build utils))
                   #~(begin
                       (use-modules (guix build utils))

                       (setenv "PATH" #+(file-append patch "/bin"))

                       ;; XXX: Assume SOURCE is a directory.  This is true in
                       ;; most practical cases, where it's a <git-checkout>.
                       (copy-recursively #+source #$output)
                       (chdir #$output)
                       (for-each (lambda (patch)
                                   (invoke "patch" "-p1" "--batch"
                                           "-i" patch))
                                 '(#+@patches))))))

(define maybe-instantiate-channel
  (match-lambda
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
                          (map instantiate-patch
                               (pk 'patches
                                   (append-map
                                    (cute find-files <> "\\.patch")
                                    (instantiate-origins patches)))))))
                   (_ (built-derivations (list drv))))
                (return (derivation->output-path drv)))))
          #:commit (channel-commit ch)
          #:url (channel-url ch)
          #:name (channel-name ch))))))


(define %channels
  (list
   (cons
    (channel
     (name 'guix)
     (branch "master")
     ;; (commit "c5fa9dd0e96493307cc76ea098a6bca9b076e012")
     (introduction
      (make-channel-introduction
       "9edb3f66fd807b096b48283debdcddccfea34bad"
       (openpgp-fingerprint
        "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))
     (url
      (if (file-exists? "/home/graves/spheres/info/.bare/guix.git")
          "/home/graves/spheres/info/.bare/guix.git"
          "https://git.savannah.gnu.org/git/guix.git")))
    (list
     (origin
      (method patchset-fetch)
      (uri (patchset-reference
            (type 'gnu) (id 65613) (version 1)))
      (sha256
       (base32
        "05vwh940ak8yv01r2gxfr1ikwk4pi4kl6wxpdm4si8ri7j4kman4")))
     (origin
      (method patchset-fetch)
      (uri (patchset-reference
            (type 'gnu) (id 69280) (version 1)))
      (sha256
       (base32
        "1bk7r203c2hsdlaq5acxi2bbnh07k7hmam2kg8dksq4jp0b5kw82")))
     (origin
      (method patchset-fetch)
      (uri (patchset-reference
            (type 'gnu) (id 69052) (version 2)))
      (sha256
       (base32
        "1fvrz8vhz3bvqf70jf51l9w3sp8ryja6fas6467y7mvnnq8nzv5g")))))

   (cons
    (channel
     (name 'nonguix)
     (url
      (if (file-exists? "/home/graves/spheres/info/.bare/nonguix.git")
          "/home/graves/spheres/info/.bare/nonguix.git"
          "https://gitlab.com/nonguix/nonguix.git"))
     (branch "master")
     ;; (commit "e026dba1dad924aa09da8a28caa343a8ace3f6c7")
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
     ;; (commit "74a3fb8378e86603bb0f70b260cbf46286693392")
     (introduction
      (make-channel-introduction
       "257cebd587b66e4d865b3537a9a88cccd7107c95"
       (openpgp-fingerprint
        "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0")))
     (url
      (if (file-exists? "/home/graves/spheres/info/.bare/rde.git")
          "/home/graves/spheres/info/.bare/rde.git"
          "https://git.sr.ht/~abcdw/rde")))
    (list
     (origin  ; org-dailies
      (method patchset-fetch)
      (uri (patchset-reference
            (type 'rde) (id 49336) (version 1)))
      (sha256
       (base32
        "1q6myc0v9l0wcbbscxqx4hr2cazj7v8a5sb5g5amf2gfhgx72910")))
     (origin  ; org-agenda-files-track
      (method patchset-fetch)
      (uri (patchset-reference
            (type 'rde) (id 44893) (version 4)))
      (sha256
       (base32
        "0kxmrqhswldlx5xgy7izna3klvw2ddv6il4ic6wn5f5z68xbk9am")))
     (origin  ; age password-store
      (method patchset-fetch)
      (uri (patchset-reference
            (type 'rde) (id 36511) (version 2)))
      (sha256
       (base32
        "1xjg8kc5i6sbcjnd9s1djl1dx9kg92il43afizg72si5pp0hfs9l")))
     (origin  ; Guix's SSH configuration
      (method patchset-fetch)
      (uri (patchset-reference
            (type 'rde) (id 40004) (version 3)))
      (sha256
       (base32
        "0d103n0vwwqc8l5mlj7sjzw402ris7qhrz6fvr95qwvhhn0i1v1a")))
     (origin  ; SSH option ssh-add-keys
      (method patchset-fetch)
      (uri (patchset-reference
            (type 'rde) (id 40007) (version 1)))
      (sha256
       (base32
        "1khdmm392v19mp1710rbk2wfm4zibnpi9knx0yh0si603f0bj1bz")))
     (origin  ; power-menu logout
      (method patchset-fetch)
      (uri (patchset-reference
            (type 'rde) (id 47815) (version 1)))
      (sha256
       (base32
        "199s4jf28x44lpha1jjjh15c00629yv4w3vw2pq70dx7gy5rsxx6")))
     (origin  ; emacs background-server mode
      (method patchset-fetch)
      (uri (patchset-reference
            (type 'rde) (id 48753) (version 1)))
      (sha256
       (base32
        "0mb2qyppisq6rq303gxa1vj4m2lw1qn5f0kv0971q0pz2c1q22va")))
     (origin  ; org-roam-todo unecessary sync
      (method patchset-fetch)
      (uri (patchset-reference
            (type 'rde) (id 44345) (version 1)))
      (sha256
       (base32
        "1390wpb2ng8x866i5yswyf3mhl6gzfscqfq82wn30c8vn9kmgk1h")))
     (origin  ; org-roam-file-exclude-regexp
      (method patchset-fetch)
      (uri (patchset-reference
            (type 'rde) (id 39539) (version 4)))
      (sha256
       (base32
        "0vckbkwh3x07p4b57pj1h6bldbsayl2cbysrc00pybl8vml7sh61")))
     (origin  ; sway focus emacs-client frames.
      (method patchset-fetch)
      (uri (patchset-reference
            (type 'rde) (id 47806) (version 1)))
      (sha256
       (base32
        "0n09agca480mcfirwgl23bmpjpc02xkm5bc82mn6bnjs9zq6kvkb")))))))

(map maybe-instantiate-channel %channels)
