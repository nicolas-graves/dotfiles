(define-module (home services mako)
  #:use-module (ice-9 match)
  #:use-module (gnu home services)
  #:use-module (gnu services configuration)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (gnu packages wm)
  #:use-module (guix import utils)
  #:use-module ((gnu home-services-utils) #:select (maybe-object->string))
  #:export (home-mako-service-type
            home-mako-configuration))

(define-configuration/no-serialization home-mako-configuration
  (package
    (package mako)
    "mako package to use")
  (config
   (alist '())
   ""))

(define (serialize-mako-config config)
  (define* (serialize-criteria criteria #:optional (res '()))
    (match criteria
      ((field value)
       (format #f "[~a]\n"
               (string-join
                (cons (format #f "~a=\"~a\"" field value) res))))
      ((field value . rest)
       (serialize-criteria
        rest
        (cons (format #f "~a=\"~a\"" field value) res)))))

  (define (serialize-key key)
    (if (list? key)
        (serialize-criteria key)
        (string-append (maybe-object->string key) "=")))

  (define (serialize-val val)
    (cond
     ((or (number? val) (symbol? val)) (maybe-object->string val))
     ((list? val) (serialize-mako-config val))
     (else val)))

  (define (serialize-field key val)
    (let ((val (serialize-val val))
          (key (serialize-key key)))
      (list key val)))

  (generic-serialize-alist
   (lambda args (flatten (interpose args)))
   serialize-field
   config))

(define (add-mako-configuration config)
  (let ((cfg (home-mako-configuration-config config)))
    `(("config/mako/config"
       ,(apply mixed-text-file
               "config"
               (serialize-mako-config cfg))))))

(define add-mako-package
  (compose list home-mako-configuration-package))

(define home-mako-service-type
  (service-type
   (name 'home-mako)
   (extensions
    (list (service-extension
           home-files-service-type
           add-mako-configuration)
          (service-extension
           home-profile-service-type
           add-mako-package)))
   (default-value (home-mako-configuration))
   (description "")))
