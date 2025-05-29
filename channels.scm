;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2021-2025 Nicolas Graves <ngraves@ngraves.fr>

(use-modules
 (guix channels)
 (guix packages)
 (guix gexp)
 (guix memoization)
 (guix records)
 (ice-9 match)
 (srfi srfi-26))

;; Declarations. These are actually defined in (guix-stack patchset)
;; and will be overwritten by the (use-modules) if available.
;; This allows us to have a functioning file even in a profile sans guix-stack.
;; Records can only be defined at top level (see manual).
(define maybe-instantiate-channel identity)
(define* (make-patched-channel channel #:optional patches) channel)
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
    (use-modules (guix-stack patchset))
    (display "Module (guix-stack patchset) loaded.\n"))
  (lambda (key . args)
    (display "Module (guix-stack patchset) not found. Falling back...\n")))

(define %cwd (dirname (current-filename)))

(define submodule
  (memoize
   (lambda (file)
     (let ((path (string-append %cwd "/channels/" file)))
       (and (file-exists? path) path)))))

;; We can also get rid of that hand-written cache, this is how :
;; guix describe caches the previous version of channels, with
;; proper introductions.
;; We can parse them in guix-stack to avoid having to define that here.
;; For new introductions, we can generate them with (git).
;; But that's a little tedious for the benefit.
(define cached-introductions
  `((guix
     . ,(make-channel-introduction
         "9edb3f66fd807b096b48283debdcddccfea34bad"
         (openpgp-fingerprint
          "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))
    (nonguix
     . ,(make-channel-introduction
         "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
         (openpgp-fingerprint
          "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))
    (rde
     . ,(make-channel-introduction
         "257cebd587b66e4d865b3537a9a88cccd7107c95"
         (openpgp-fingerprint
          "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0")))
    (guix-past
     . ,(make-channel-introduction
         "0c119db2ea86a389769f4d2b9c6f5c41c027e336"
         (openpgp-fingerprint
          "3CE4 6455 8A84 FDC6 9DB4  0CFB 090B 1199 3D9A EBB5")))
    (guix-science
     . ,(make-channel-introduction
	 "b1fe5aaff3ab48e798a4cce02f0212bc91f423dc"
	 (openpgp-fingerprint
	  "CA4F 8CF4 37D7 478F DA05  5FD4 4213 7701 1A37 8446")))
    (guix-science-nonfree
     . ,(make-channel-introduction
	 "58661b110325fd5d9b40e6f0177cc486a615817e"
	 (openpgp-fingerprint
	  "CA4F 8CF4 37D7 478F DA05  5FD4 4213 7701 1A37 8446")))))

(define %channels
  (list
   (make-patched-channel
    (channel
     (name 'guix)
     (branch "master")
     (commit (and (not (submodule "guix"))
                  "2dc38e493beaabb3f8d8c8b646a9374efc17db67"))
     (introduction (assoc-ref cached-introductions 'guix))
     (url
      (or (submodule "guix")
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
     (name 'rde)
     (branch "master")
     (commit (and (not (submodule "rde"))
                  "bc3d6ea1fef988c0d8c1bd5bf0ab0ae83c148251"))
     (introduction (assoc-ref cached-introductions 'rde))
     (url
      (or (submodule "rde")
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
     (local-file "patches/rde-project-Disable-broken-configuration.patch")))))

;; (map maybe-instantiate-channel %channels)

(catch #t
  (lambda ()
    (use-modules (guix-stack channel-submodules))
    (submodules-dir->channels "channels" #:use-local-urls? #t))
  (lambda (key . args)
    (display "(guix-stack channel-submodules) not found. Falling back...\n")
    (load (string-append
           %cwd "/channels/guix-stack/src/guix-stack/channel-submodules.scm"))
    (use-modules (guix-stack channel-submodules))
    (submodules-dir->channels "channels" #:use-local-urls? #t)))
