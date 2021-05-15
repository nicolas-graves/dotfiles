(use-modules (gnu packages)
             (gnu packages emacs-xyz)
             (guix packages)
             (guix git-download)
             (guix build-system emacs)
             (guix transformations)
             (guix profiles)
             ((guix licenses) #:prefix license:))


(define transform
  (options->transformation
   '((with-commit . "emacs-evil=8dc0ccdc4c0246af87588a93812a23268f83ab94")
     (with-commit . "emacs-icomplete-vertical=d7ab5e5de18a027375666755e6610ea26c35ac16")
     (with-commit . "emacs-magit=471c63d92ce22b8ea653f821bc1893ecea324d4d")
     (with-commit . "emacs-consult=8b2daa697a29ae1a2b26ee54874354d2913e4d5c")
     (with-commit . "emacs-use-package=a7422fb8ab1baee19adb2717b5b47b9c3812a84c"))))


(define emacs-flymake-quickdef
  (package
    (name "emacs-flymake-quickdef")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/karlotness/flymake-quickdef")
                    (commit (string-append "v" version))))
              (sha256
               (base32 "19gfd539l97j8xbrq1fw83b54mxbcamlz9m896088d3p01zf8b0g"))))
    (build-system emacs-build-system)
    (synopsis "")
    (description "")
    (license license:gpl3)
    (home-page "https://github.com/karlotness/flymake-quickdef")))


(define emacs-flymake-kondor
  (package
    (name "emacs-flymake-kondor")
    (version "0.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/turbo-cafe/flymake-kondor")
                    (commit version)))
              (sha256
               (base32 "0h8dqk35r10pxx2w4swb3kij4y2vi17j9wfk978x8lf0wd3h3hsy"))))
    (propagated-inputs `(("emacs-flymake-quickdef" ,emacs-flymake-quickdef)))
    (build-system emacs-build-system)
    (synopsis "")
    (description "")
    (license license:gpl3)
    (home-page "https://github.com/turbo-cafe/flymake-kondor")))


(define emacs-flymake-posframe
  (package
    (name "emacs-flymake-posframe")
    (version "1.0.0.fcde57c")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/krevedkokun/flymake-posframe")
                    (commit "fcde57c3d3e7d9655cb0df55d55dc963bf722d9f")))
              (sha256
               (base32 "08id06c3750adxgk8y30yai098jzrh0mmgwn34b7gmfvcn16934n"))))
    (propagated-inputs `(("emacs-posframe" ,emacs-posframe)))
    (build-system emacs-build-system)
    (synopsis "")
    (description "")
    (license license:gpl3)
    (home-page "https://github.com/krevedkokun/flymake-posframe")))


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


(define emacs-flimenu
  (package
    (name "emacs-flimenu")
    (version "4c0ff37")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/IvanMalison/flimenu")
                    (commit "4c0ff37cf3bd6c836bd136b5f6c450560a6c92b9")))
              (sha256
               (base32 "1z57vm8pgxfhklzz4gpyqy8wwv3dzl76dzgilx5pfkd69m2jrijg"))))
    (build-system emacs-build-system)
    (synopsis "")
    (description "")
    (license license:gpl3)
    (home-page "https://github.com/IvanMalison/flimenu")))


(define emacs-ranger-el
  (package
    (name "emacs-ranger-el")
    (version "0.9.8.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ralesi/ranger.el")
                    (commit (string-append "v" version))))
              (sha256
               (base32 "01rphv92g1r0cw5bwkbrh02s0na7fjrddxx1dckk2y7qr97s7l8j"))))
    (build-system emacs-build-system)
    (synopsis "")
    (description "")
    (license license:gpl3)
    (home-page "https://github.com/ralesi/ranger.el")))


(packages->manifest
 (append
  (list emacs-geiser-eros
        emacs-flymake-kondor
        emacs-flymake-posframe
        emacs-flimenu
        emacs-ranger-el)
  (map specification->package
       '("emacs-next-pgtk"
         "emacs-leaf"
         "emacs-smartparens"
         "emacs-orderless"
         "emacs-modus-themes"
         "emacs-geiser"
         "emacs-geiser-guile"
         "emacs-which-key"
         "emacs-eros"
         "emacs-gcmh"
         "emacs-minions"
         "emacs-async"
         "emacs-marginalia"
         "emacs-rg"
         "emacs-nov-el"
         "emacs-pdf-tools"
         "emacs-eglot"
         "emacs-docker"
         "emacs-dockerfile-mode"
         "emacs-docker-compose-mode"
         "emacs-restclient"
         "emacs-macrostep"
         "emacs-csv-mode"
         "emacs-consult"
         "emacs-project"
         "emacs-erc-image"
         "emacs-erc-hl-nicks"
         "emacs-clojure-mode"
         "emacs-cider"
         ))
  (map (compose transform specification->package)
       '("emacs-use-package"
         "emacs-evil"
         "emacs-evil-collection"
         "emacs-evil-cleverparens"
         "emacs-evil-commentary"
         "emacs-evil-multiedit"
         "emacs-evil-surround"
         "emacs-magit"
         "emacs-icomplete-vertical"
         ))))

;; guix package --profile=$GUIX_EXTRA_PROFILES/emacs/emacs --manifest=$HOME/.config/guix/manifests/emacs.scm
