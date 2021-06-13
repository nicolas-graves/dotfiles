(define-module (manifests emacs)
  #:use-module (gnu packages)

  #:use-module (guix profiles)
  #:use-module (guix transformations))


(define transform
  (options->transformation
   '((with-commit . "emacs-evil=8dc0ccdc4c0246af87588a93812a23268f83ab94")
     (with-commit . "emacs-icomplete-vertical=d7ab5e5de18a027375666755e6610ea26c35ac16")
     (with-commit . "emacs-consult=8b2daa697a29ae1a2b26ee54874354d2913e4d5c")
     (with-commit . "emacs-use-package=a7422fb8ab1baee19adb2717b5b47b9c3812a84c"))))


(packages->manifest
 (append
  (map specification->package
       '("emacs-next-pgtk"
         "emacs-leaf"
         "emacs-smartparens"
         "emacs-orderless"
         "emacs-modus-themes"
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
         "emacs-helpful"
         "emacs-geiser"
         "emacs-geiser-guile"
         "emacs-moody"
         "emacs-magit"
         "emacs-geiser-eros"
         "emacs-flymake-kondor"
         "emacs-flimenu"
         ))
  (map (compose transform specification->package)
       '("emacs-use-package"
         "emacs-evil"
         "emacs-evil-collection"
         "emacs-evil-cleverparens"
         "emacs-evil-commentary"
         "emacs-evil-multiedit"
         "emacs-evil-surround"
         "emacs-icomplete-vertical"
         "emacs-kubel"
         ))))

;; guix package --profile=$GUIX_EXTRA_PROFILES/emacs/emacs --manifest=$HOME/.config/guix/manifests/emacs.scm
