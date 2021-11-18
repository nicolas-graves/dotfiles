(define-module (home services i3blocks)
  #:use-module (gnu home services)
  #:use-module ((gnu home-services-utils)
                #:select (ini-config?
                          maybe-object->string
                          generic-serialize-ini-config))
  #:use-module (gnu services configuration)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (gnu packages wm)
  #:use-module (guix import utils)
  #:export (home-i3blocks-service-type
            home-i3blocks-configuration))

(define-configuration/no-serialization home-i3blocks-configuration
  (package
    (package i3blocks)
    "i3blocks package to use")
  (config
   (ini-config '())
   ""))

(define (serialize-i3blocks-config config)
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

(define (add-i3blocks-configuration config)
  (let ((cfg (home-i3blocks-configuration-config config)))
    `(("config/i3blocks/config"
       ,(apply mixed-text-file
               "config"
               (serialize-i3blocks-config cfg))))))

(define add-i3blocks-package
  (compose list home-i3blocks-configuration-package))

(define home-i3blocks-service-type
  (service-type
   (name 'home-i3blocks)
   (extensions
    (list (service-extension
           home-files-service-type
           add-i3blocks-configuration)
          (service-extension
           home-profile-service-type
           add-i3blocks-package)))
   (default-value (home-i3blocks-configuration))
   (description "")))
