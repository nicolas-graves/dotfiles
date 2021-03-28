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
   '((with-commit . "emacs-evil=cc9d6886b418389752a0591b9fcb270e83234cf9")
     (with-commit . "emacs-evil-collection=458d6bd0f2a48a5986fb93e624f9720707078ab6")
     (with-commit . "emacs-icomplete-vertical=e490b01f7420bc15bc8e7b4594964208c3d31107")
     (with-commit . "emacs-magit=68f3753823aa1423b50d6a90e9fa2066361e8306"))))


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


(packages->manifest
 `(,emacs-geiser-eros
   ,emacs-flymake-kondor
   ,emacs-flymake-posframe
   ,emacs-flimenu
   ,@(map specification->package
          '(
            "emacs-next"
            "emacs-leaf"
            "emacs-smartparens"
            "emacs-orderless"
            "emacs-modus-themes"
            "emacs-geiser"
            "emacs-which-key"
            "emacs-eros"
            "emacs-gcmh"
            "emacs-minions"
            "emacs-clojure-mode"
            "emacs-cider"
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
            ))
   ,@(map (compose transform specification->package)
          '("emacs-evil"
            "emacs-evil-collection"
            "emacs-evil-cleverparens"
            "emacs-evil-commentary"
            "emacs-evil-multiedit"
            "emacs-evil-surround"
            "emacs-magit"
            "emacs-icomplete-vertical"
            ))))

;; guix package --profile=$GUIX_EXTRA_PROFILES/emacs/emacs --manifest=$HOME/.config/guix/manifests/emacs.scm
