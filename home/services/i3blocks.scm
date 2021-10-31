(define-module (home services i3blocks)
  #:use-module (gnu home services)
  #:use-module (gnu home-services-utils)
  #:use-module ((gnu services configuration)
                #:select (define-configuration/no-serialization))
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

(define (serialize-hg-config config)
  (define (serialize-boolean val)
    (list (if val "True" "False")))

  (define (serialize-list val)
    (interpose (map serialize-val val) ", "))

  (define (serialize-val val)
    (cond
     ((list? val) (serialize-list val))
     ((boolean? val) (serialize-boolean val))
     ((or (number? val) (symbol? val)) (list (maybe-object->string val)))
     (else (list val))))

  (define (serialize-field key val)
    (let ((val (serialize-val val))
          (key (symbol->string key)))
      `(,key "=" ,@val "\n")))

  (flatten (generic-serialize-ini-config
            #:combine-ini interpose
            #:combine-alist list
            #:combine-section-alist cons
            #:serialize-field serialize-field
            #:fields config)))

(define (add-i3blocks-configuration config)
  (let ((cfg (home-i3blocks-configuration-config config)))
    `(("config/i3blocks/config"
       ,(apply mixed-text-file
               "config"
               (serialize-hg-config cfg))))))

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
