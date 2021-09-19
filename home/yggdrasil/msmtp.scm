(define-module (home yggdrasil msmtp)
  #:use-module (gnu home-services)
  #:use-module (gnu services configuration)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (gnu packages mail)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (gnu home-services-utils)
  #:export (home-msmtp-service-type
            home-msmtp-configuration))

(define (serialize-isync-config field-name val)
  (define (serialize-term term)
    (match term
      ((? symbol? e) (symbol->string e))
      ((? number? e) (format #f "~a" e))
      ((? string? e) (format #f "~s" e))
      (e e)))
  (define (serialize-item entry)
    (match entry
      ((? gexp? e) e)
      ((? list lst)
       #~(string-join '#$(map serialize-term lst)))))

  #~(string-append #$@(interpose (map serialize-item val) "\n" 'suffix)))

(define-configuration/no-serialization home-msmtp-configuration
  (package
    (package msmtp)
    "msmtp package to use")
  (xdg-flavor?
   (boolean #t)
   "")
  (config
   (list '())
   ""))

(define (add-msmtp-configuration config)
  (let ((cfg (home-msmtp-configuration-config config)))
    `(("config/msmtp/config"
       ,(mixed-text-file
         "config"
         (serialize-isync-config #f cfg))))))

(define add-msmtp-package
  (compose list home-msmtp-configuration-package))

(define home-msmtp-service-type
  (service-type
   (name 'home-msmtp)
   (extensions
    (list (service-extension
           home-files-service-type
           add-msmtp-configuration)
          (service-extension
           home-profile-service-type
           add-msmtp-package)))
   (default-value (home-msmtp-configuration))
   (description "")))
