(define-module (home yggdrasil emacs)
  #:use-module (guix gexp)
  #:use-module (guix transformations)

  #:use-module (gnu packages)
  #:use-module (rde packages)

  #:use-module (gnu home services)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home-services-utils)
  #:use-module ((home yggdrasil emacs-packages) #:prefix emacs:))

(define-public services
  (list
   (service home-emacs-service-type
            (home-emacs-configuration
             (package emacs-pgtk-native-comp)
             (rebuild-elisp-packages? #f)
             ;;(server-mode? #t)
             (init-el
              `(,(slurp-file-gexp (local-file "../../../.config/emacs/init.el"))))
             (elisp-packages emacs:packages)))))
