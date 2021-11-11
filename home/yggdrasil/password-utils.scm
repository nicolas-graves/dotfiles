(define-module (home yggdrasil password-utils)
  #:use-module (gnu services)
  #:use-module (gnu home-services state)
  #:use-module (gnu home-services password-utils))

(define-public services
  (list
   (simple-service
    'add-password-store-git-state
    home-state-service-type
    (list
     (state-git
      (string-append (getenv "XDG_STATE_HOME") "/password-store")
      "git@git.sr.ht:~krevedkokun/pass")))
   (service
    home-password-store-service-type
    (home-password-store-configuration
     (browserpass-native? #t)))))
