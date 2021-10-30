(define-module (home yggdrasil emacs)
  #:use-module (guix gexp)
  #:use-module (guix transformations)

  #:use-module (gnu packages)
  #:use-module (rde packages)

  #:use-module (gnu home services)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home-services-utils))

(define transform
  (options->transformation
   '((with-commit . "emacs-use-package=a7422fb8ab1baee19adb2717b5b47b9c3812a84c"))))

(define packages
  (map (compose transform specification->package)
       '("emacs-orderless"
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
         "emacs-magit"
         "emacs-geiser-eros"
         "emacs-flymake-kondor"
         "emacs-use-package"
         "emacs-direnv"
         "emacs-mini-frame"
         "emacs-embark"
         "emacs-sly"
         "emacs-paredit"
         "emacs-notmuch"
         "emacs-vertico"
         "emacs-corfu"
         )))

(define-public services
  (list
   (service home-emacs-service-type
            (home-emacs-configuration
             (package emacs-next-pgtk-latest)
             (rebuild-elisp-packages? #t)
             (init-el
              `(,(slurp-file-gexp (local-file "files/init.el"))))
             (elisp-packages packages)))))
