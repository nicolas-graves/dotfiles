(define-module (home services swappy)
  #:use-module (gnu home services)
  #:use-module ((gnu home-services-utils)
                #:select (ini-config?
                          maybe-object->string
                          generic-serialize-ini-config))
  #:use-module (gnu services configuration)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (gnu packages image)
  #:use-module (guix import utils)
  #:export (home-swappy-configuration
            home-swappy-service-type))

(define-configuration/no-serialization home-swappy-configuration
  (package
    (package swappy)
    "swappy package to use")
  (config
   (ini-config '())
   ""))

(define (serialize-swappy-config config)
  (define (serialize-val val)
    (if (or (number? val) (symbol? val))
        (maybe-object->string val)
        val))

  (define (serialize-field key val)
    (let ((val (serialize-val val))
          (key (symbol->string key)))
      (list key "=" val "\n")))

  (generic-serialize-ini-config
   #:combine-ini (compose flatten interpose)
   #:combine-alist list
   #:combine-section-alist cons
   #:serialize-field serialize-field
   #:fields config))

(define (add-swappy-configuration config)
  (let ((cfg (home-swappy-configuration-config config)))
    `(("config/swappy/config"
       ,(apply mixed-text-file
               "config"
               (serialize-swappy-config cfg))))))

(define add-swappy-package
  (compose list home-swappy-configuration-package))

(define home-swappy-service-type
  (service-type
   (name 'home-swappy)
   (extensions
    (list (service-extension
           home-files-service-type
           add-swappy-configuration)
          (service-extension
           home-profile-service-type
           add-swappy-package)))
   (default-value (home-swappy-configuration))
   (description "")))
