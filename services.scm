(define-module (services)
  #:use-module (ice-9 match)

  #:use-module (guix gexp)
  #:use-module (guix packages)

  #:use-module (gnu system setuid)

  #:use-module (gnu services)
  #:use-module ((gnu services configuration)
                #:select (define-configuration/no-serialization))
  #:use-module (gnu services shepherd)
  #:use-module (gnu services dbus)

  #:use-module (gnu home-services-utils)

  #:use-module (gnu packages base)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages networking)

  #:export (iwd-configuration
            iwd-service-type
            opendoas-configuration
            opendoas-service-type))

(define-configuration/no-serialization iwd-configuration
  (package (package iwd) "")
  (config (ini-config '()) ""))

(define (iwd-shepherd-service config)
  "Return a shepherd service for iwd"
  (let ((pkg (iwd-configuration-package config))
        (environment #~(list (string-append
                              "PATH="
                              (string-append #$openresolv "/sbin")
                              ":"
                              (string-append #$coreutils "/bin")))))
    (list
     (shepherd-service
      (documentation "Run iwd")
      (provision '(networking))
      (requirement '(user-processes dbus-system loopback))
      (start #~(make-forkexec-constructor
                (list (string-append #$pkg "/libexec/iwd"))
                #:log-file "/var/log/iwd.log"
                #:environment-variables #$environment))
      (stop #~(make-kill-destructor))))))

(define (iwd-etc-service config)
  (define (serialize-field key val)
    (let ((val (cond
                ((list? val) (string-join (map maybe-object->string val) ";"))
                (else val))))
      (format #f "~a=~a\n" key val)))

  (let ((cfg (iwd-configuration-config config)))
    `(("iwd/main.conf"
       ,(mixed-text-file
         "main.conf"
         (generic-serialize-ini-config
          #:serialize-field serialize-field
          #:fields cfg))))))

(define iwd-service-type
  (let ((iwd-package (const (list iwd))))
    (service-type
     (name 'iwd)
     (extensions
      (list (service-extension
             shepherd-root-service-type
             iwd-shepherd-service)
            (service-extension
             dbus-root-service-type
             iwd-package)
            (service-extension
             etc-service-type
             iwd-etc-service)
            (service-extension
             profile-service-type
             iwd-package)))
     (default-value (iwd-configuration))
     (description ""))))

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
