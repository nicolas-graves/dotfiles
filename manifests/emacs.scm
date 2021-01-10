(use-modules
 (gnu packages)
 (gnu packages emacs-xyz)
 (guix profiles)
 (guix packages)
 (guix git-download)
 (guix build-system emacs)
 (guix transformations)
 ((guix licenses) #:prefix license:))


(define transform
  (options->transformation
   '((with-commit . "emacs-evil=cc9d6886b418389752a0591b9fcb270e83234cf9")
     (with-commit . "emacs-evil-collection=be07f6a2905494a97215fa236f3bf40f945dfcea")
     (with-commit . "emacs-icomplete-vertical=0.3"))))


(define emacs-flymake-posframe
  (package
    (name "emacs-flymake-posframe")
    (version "96a1893")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Ladicle/flymake-posframe")
                    (commit "96a1893eb9d883e88f4b16a0199c8de5131260ed")))
              (sha256
               (base32 "0s7dz3nwjjrwibipvpxpxksa8n5vdxqczpg53iyyg5hmv8wifrk9"))))
    (propagated-inputs `(("emacs-posframe" ,emacs-posframe)))
    (build-system emacs-build-system)
    (synopsis "")
    (description "")
    (license license:gpl3)
    (home-page "https://github.com/Ladicle/flymake-posframe")))


(define emacs-geiser-eros
  (package
    (name "emacs-geiser-eros")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.sr.ht/~sokolov/geiser-eros")
                    (commit version)))
              (sha256
               (base32 "119s7xa7cw7gxhibxpfxi103izz268840f8xn2x5d9gd0f8s9gcn"))))
    (propagated-inputs `(("emacs-eros" ,emacs-eros)
                         ("emacs-geiser" ,emacs-geiser)))
    (build-system emacs-build-system)
    (synopsis "")
    (description "")
    (license license:gpl3)
    (home-page "https://git.sr.ht/~sokolov/geiser-eros")))


(define emacs-marginalia
  (package
    (name "emacs-marginalia")
    (version "aa41183")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/minad/marginalia")
                    (commit "aa41183d7ce3ea7b01c06ea52e90c548ea039107")))
              (sha256
               (base32 "1jkzyhix9szrmr0rrwhbqb8vqbg9g4vjm5l5103fxqrssyp132k2"))))
    (build-system emacs-build-system)
    (synopsis "")
    (description "")
    (license license:gpl3)
    (home-page "https://github.com/minad/marginalia")))


(define emacs-consult
  (package
    (name "emacs-consult")
    (version "3c4a1f6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/minad/consult")
                    (commit "3c4a1f69170672a02a5d3280afd04f312d4ddb34")))
              (sha256
               (base32 "1aqy4cak2am7zk5k555i0sfvgrg4s35dny2cqqdwrxn4k0a20kaz"))))
    (build-system emacs-build-system)
    (arguments `(#:include '("consult.el" "consult-flymake.el")))
    (synopsis "")
    (description "")
    (license license:gpl3)
    (home-page "https://github.com/minad/consult")))


(packages->manifest
 `(,emacs-geiser-eros
   ,emacs-marginalia
   ,emacs-consult
   ,@(map specification->package
          '("emacs-next"
            "emacs-leaf"
            "emacs-smartparens"
            "emacs-orderless"
            "emacs-modus-themes"
            "emacs-geiser"
            "emacs-which-key"
            "emacs-eros"
            "emacs-gcmh"
            "emacs-minions"
            "emacs-magit"
            "emacs-clojure-mode"
            "emacs-cider"
            "emacs-async"))
   ,@(map (compose transform specification->package)
          '("emacs-evil"
            "emacs-evil-collection"
            "emacs-evil-cleverparens"
            "emacs-evil-commentary"
            "emacs-evil-multiedit"
            "emacs-evil-surround"
            "emacs-icomplete-vertical"))) )

;; guix package --profile=$GUIX_EXTRA_PROFILES/emacs/emacs --manifest=$HOME/.config/guix/manifests/emacs.scm
