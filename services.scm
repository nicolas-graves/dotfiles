(define-module (services)
  #:use-module (ice-9 match)

  #:use-module (guix gexp)
  #:use-module (guix packages)

  #:use-module (gnu system setuid)

  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services dbus)

  #:use-module ((gnu home-services-utils)
                #:select (ini-config?
                          maybe-object->string
                          generic-serialize-ini-config))

  #:use-module (gnu packages base)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages networking)

  #:export (opendoas-configuration
            opendoas-service-type))

(define opendoas-config? list?)

(define-configuration/no-serialization opendoas-configuration
  (package (package opendoas) "")
  (config (opendoas-config '()) ""))

(define (serialize-opendoas-config config)
  (define (serialize-opendoas-env env)
    (map (match-lambda
           ((var . val) (format #f "~a=~a" var val))
           ((? symbol? val) (symbol->string val)))
         env))

  (define (serialize-opendoas-term term)
    (match term
      ((? symbol? e) (symbol->string e))
      ((? list? e) (format #f "{ ~a }" (string-join (serialize-opendoas-env e))))
      ((? string? e) e)
      (e e)))

  (define (serialize-opendoas-line line)
    #~(string-join '#$(map serialize-opendoas-term line)))

  #~(string-append #$@(interpose (map serialize-opendoas-line config) "\n" 'suffix)))

(define (add-opendoas-config config)
  (let ((cfg (opendoas-configuration-config config)))
    `(("doas.conf"
       ,(mixed-text-file
         "doas.conf"
         (serialize-opendoas-config cfg))))))

(define add-opendoas-package
  (compose list opendoas-configuration-package))

(define (add-opendoas-setuid-programs config)
  (let ((pkg (opendoas-configuration-package config)))
    `(,(file-like->setuid-program (file-append pkg "/bin/doas")))))

(define opendoas-service-type
  (service-type
   (name 'opendoas)
   (extensions
    (list (service-extension
           etc-service-type
           add-opendoas-config)
          (service-extension
           profile-service-type
           add-opendoas-package)
          (service-extension
           setuid-program-service-type
           add-opendoas-setuid-programs)))
   (default-value (opendoas-configuration))
   (description "")))

;; permit persist setenv { PKG_CACHE PKG_PATH } aja cmd pkg_add
;; permit setenv { -ENV PS1=$DOAS_PS1 SSH_AUTH_SOCK } :wheel
;; permit nopass tedu as root cmd /usr/sbin/procmap
;; permit nopass keepenv setenv { PATH } root as root
