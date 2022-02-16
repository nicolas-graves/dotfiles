(define-module (home yggdrasil emacs core)
  #:use-module (guix gexp)
  #:use-module (guix transformations)

  #:use-module (gnu packages)
  #:use-module (flat packages emacs)

  #:use-module (gnu home services)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home-services-utils)
  #:use-module (home yggdrasil emacs packages))


(define-public services
  (list
   (service
    home-emacs-service-type
    (home-emacs-configuration
     (package emacs-pgtk-native-comp)
     (rebuild-elisp-packages? #f)
     ;;(server-mode? #t)
     (init-el
      `(,(slurp-file-gexp (local-file "keyboard.el"))
        ,(slurp-file-gexp (local-file "init.el"))
        ,(slurp-file-gexp (local-file "ui.el"))
        ,(slurp-file-gexp (local-file "org.el"))
        ,(slurp-file-gexp (local-file "workflow.el"))

        ))
     (elisp-packages packages)))))
