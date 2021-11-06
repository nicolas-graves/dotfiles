(define-module
  (home yggdrasil stow)
  #:use-module
  (gnu home)
  #:use-module
  (gnu home services)
  #:use-module
  (guix packages)
  #:use-module
  (guix gexp)
  #:use-module
  (ice-9 match)
  #:use-module
  (srfi srfi-1)
  #:use-module
  (gnu home-services-utils)
  )

(define-public services
  (list
   (service home-files-service-type
            (list
             `("local/bin" ,(local-file "scripts" #:recursive? #t))
             `("local/share" ,(local-file "share" #:recursive? #t))
             ))))
