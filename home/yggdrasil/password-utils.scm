(define-module (home yggdrasil password-utils)
  #:use-module (gnu services)
  #:use-module (gnu home-services state)
  #:use-module (gnu home-services password-utils))

(define-public services
  (list
   (service home-password-store-service-type
            (home-password-store-configuration
             (browserpass-native? #t)))))
