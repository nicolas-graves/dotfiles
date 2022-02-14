(define-module (home yggdrasil bootstrap)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (home yggdrasil rbw)
  #:use-module (home yggdrasil ssh)
  #:use-module ((home yggdrasil packages) #:select (packages))
  #:use-module (gnu home-services-utils))

(define services
  (list
   (service
    home-files-service-type
    (append
        (append
            (list
             `("ssh/id_rsa.pub" ,(local-file "../../keys/id_rsa.pub"))
             `("ssh/id_ed25519.pub" ,(local-file "../../keys/id_ed25519.pub"))
             `("ssh/id_rsa_git.pub" ,(local-file "../../keys/id_rsa_git.pub"))
             `("config/guix/channels.scm" ,(local-file "../../channels.scm")))
            rbw-config)
        known-hosts-config))))

(home-environment
 (packages packages)
 (services services))
