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
     (with-commit . "emacs-selectrum=b290d4bdfb4da30d85ccd08347bfb8cecbf3d3db")
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


(packages->manifest
 `(,emacs-geiser-eros
   ,emacs-flymake-kondor
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            "emacs-restclient"
            "emacs-ob-restclient"
            "emacs-macrostep"
            ))
   ,@(map (compose transform specification->package)
          '("emacs-evil"
            "emacs-evil-collection"
            "emacs-evil-cleverparens"
            "emacs-evil-commentary"
            "emacs-evil-multiedit"
            "emacs-evil-surround"
            "emacs-magit"
            "emacs-selectrum"
            ))))

;; guix package --profile=$GUIX_EXTRA_PROFILES/emacs/emacs --manifest=$HOME/.config/guix/manifests/emacs.scm
