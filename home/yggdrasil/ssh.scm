(define-module (home yggdrasil ssh)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services ssh))

(define-public services
  (list
   (service home-ssh-service-type)))
