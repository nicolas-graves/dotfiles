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

(define %channels
  (list
   (make-patched-channel
    (channel
     (name 'guix)
     (branch "master")
     (commit (and (not (submodule "guix"))
                  "2dc38e493beaabb3f8d8c8b646a9374efc17db67"))
     (introduction
      (make-channel-introduction
       "9edb3f66fd807b096b48283debdcddccfea34bad"
       (openpgp-fingerprint
        "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))
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
     (name 'nonguix)
     (url
      (or (submodule "nonguix")
          "https://gitlab.com/nonguix/nonguix.git"))
     (branch "master")
     (commit (and (not (submodule "nonguix"))
                  "6e864249c2025863e18e42587cb42764a99bec27"))
     (introduction
      (make-channel-introduction
       "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
       (openpgp-fingerprint
        "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))

   (make-patched-channel
    (channel
     (name 'rde)
     (branch "master")
     (commit (and (not (submodule "rde"))
                  "bc3d6ea1fef988c0d8c1bd5bf0ab0ae83c148251"))
     (introduction
      (make-channel-introduction
       "257cebd587b66e4d865b3537a9a88cccd7107c95"
       (openpgp-fingerprint
        "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0")))
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
     (local-file "patches/rde-project-Disable-broken-configuration.patch")))
   (make-patched-channel
    (channel
     (name 'odf-dsfr)
     (branch "master")
     (commit (and (not (submodule "odf-dsfr"))
                  "af1b66927f2dc968549a978626150b5f2c1afd37"))
     (url
      (or (submodule "odf-dsfr")
          "https://github.com/codegouvfr/odf-dsfr"))))
   (make-patched-channel
    (channel
     (name 'guix-rde)
     (branch "master")
     (commit (and (not (submodule "guix-rde"))
                  "97a32354e796324937da35fb6d430fde382fb2fe"))
     (url
      (or (submodule "guix-rde")
          "https://git.sr.ht/~ngraves/guix-rde"))))
   (make-patched-channel
    (channel
     (name 'guix-science)
     (branch "master")
     (commit (and (not (submodule "guix-science"))
                  "be44985a2d468ed8bcc09ab4bf320a4e3b6c09be"))
     (url
      (or (submodule "guix-science")
          "https://codeberg.org/guix-science/guix-science"))))
   (make-patched-channel
    (channel
     (name 'guix-science-nonfree)
     (branch "master")
     (commit (and (not (submodule "guix-science-nonfree"))
                  "5b8c3f38ee81dd090ca5fdc531eecde248c37c86"))
     (url
      (or (submodule "guix-science-nonfree")
          "https://codeberg.org/guix-science/guix-science-nonfree"))))
   (make-patched-channel
    (channel
     (name 'guix-past)
     (branch "master")
     (commit (and (not (submodule "guix-past"))
                  "2d3485b7fd7c1904bc7c1a87fc45048376ff4d3a"))
     (url
      (or (submodule "guix-past")
          "https://codeberg.org/guix-science/guix-past"))))
   (make-patched-channel
    (channel
     (name 'guix-stack)
     (branch "master")
     (commit (and (not (submodule "guix-stack"))
                  "67c456cf24e654234ff9e8642d6cf4ac916801fc"))
     (url
      (or (submodule "guix-stack")
          "https://git.sr.ht/~ngraves/guix-stack"))))
   (make-patched-channel
    (channel
     (name 'nrepl-python)
     (branch "master")
     (commit (and (not (submodule "nrepl-python"))
                  "67c456cf24e654234ff9e8642d6cf4ac916801fc"))
     (url
      (or (submodule "nrepl-python")
          "https://git.sr.ht/~ngraves/nrepl-python"))))
   (make-patched-channel
    (channel
     (name 'guix-local)
     (branch "master")
     (commit (and (not (submodule "guix-local"))
                  "b5068789e1ad096b9531018ca48a398efe43da63"))
     (url
      (or (submodule "guix-local")
          "https://git.sr.ht/~ngraves/guix-local"))))))

