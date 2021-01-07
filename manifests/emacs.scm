(use-modules
 (gnu packages)
 (gnu packages emacs-xyz)
 (guix profiles)
 (guix packages)
 (guix git-download)
 (guix build-system emacs)
 ((guix licenses) #:prefix license:))


(define emacs-evil-git
  (package/inherit
   emacs-evil
   (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/emacs-evil/evil")
                   (commit "d6cf6680ec52733ea78dc530ed75fadc5171c758")))
             (sha256
              (base32 "14rl6jx7cj336raxbksh3r2cplyifz8dghdhqvf7h1ng10sd9j6b"))))))


(define emacs-icomplete-vertical-git
  (package/inherit
   emacs-icomplete-vertical
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/oantolin/icomplete-vertical")
                  (commit "a4c65f213bd3d8be94fe8cb28ecf7ff3b44405d1")))
            (sha256
             (base32 "02v190pb802vck7di39jyf5prvmfsgcxln8mgwsls2b4clx9da97"))))))


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


(define emacs-evil-surround'
  (package/inherit
   emacs-evil-surround
   (propagated-inputs `(("emacs-evil" ,emacs-evil-git)))))

(define emacs-evil-commentary'
  (package/inherit
   emacs-evil-commentary
   (propagated-inputs `(("emacs-evil" ,emacs-evil-git)))))

(define emacs-evil-multiedit'
  (package/inherit
   emacs-evil-multiedit
   (propagated-inputs `(("emacs-evil" ,emacs-evil-git)
			("emacs-iedit" ,emacs-iedit)))))

(define emacs-evil-collection'
  (package/inherit
   emacs-evil-collection
   (version "0.0.4")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/emacs-evil/evil-collection")
                  (commit version)))
            (sha256
             (base32 "0ydr06krlj6v1r7hq3m5iy875mhmjr9amcfjjgqglfnzhic29g18"))))
   (propagated-inputs `(("emacs-evil" ,emacs-evil-git)
			("emacs-annalist" ,emacs-annalist)))))

(define emacs-evil-cleverparens'
  (package/inherit
   emacs-evil-cleverparens
   (propagated-inputs `(("emacs-evil" ,emacs-evil-git)
			("emacs-paredit" ,emacs-paredit)
			("emacs-smartparens" ,emacs-smartparens)))))

(concatenate-manifests
 (list
  (packages->manifest
   (list emacs-evil-git
         emacs-flymake-posframe
         emacs-geiser-eros
         emacs-icomplete-vertical-git
         emacs-marginalia
         emacs-consult
	 emacs-evil-surround'
         emacs-evil-multiedit'
         emacs-evil-commentary'
         emacs-evil-cleverparens'
         emacs-evil-collection'))
  (specifications->manifest
   '("emacs-next"
     "emacs-leaf"
     ;; "emacs-evil-collection"
     ;; "emacs-evil-cleverparens"
     ;; "emacs-evil-commentary"
     ;; "emacs-evil-multiedit"
     ;; "emacs-evil-surround"
     "emacs-orderless"
     ;; "emacs-icomplete-vertical"
     "emacs-modus-themes"
     "emacs-geiser"
     "emacs-which-key"
     ;; "emacs-posframe"
     "emacs-eros"
     "emacs-gcmh"
     "emacs-minions"
     "emacs-magit"
     "emacs-clojure-mode"
     "emacs-cider"
     "emacs-async"))))

;; guix package --profile=$GUIX_EXTRA_PROFILES/emacs/emacs --manifest=$HOME/.config/guix/manifests/emacs.scm
