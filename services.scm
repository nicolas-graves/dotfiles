(define-module (services)
  #:use-module (ice-9 match)

  #:use-module (guix gexp)
  #:use-module (guix packages)

  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services dbus)

  #:use-module (gnu home-services-utils)

  #:use-module (gnu packages networking)

  #:use-module (packages)

  #:export (iwd-configuration))

(define-configuration/no-serialization iwd-configuration
  (package (package iwd) "")
  (config (ini-config '()) ""))


(define (iwd-shepherd-service config)
  "Return a shepherd service for iwd"
  (let ((pkg (iwd-configuration-package config)))
    (list (shepherd-service
           (documentation "Run iwd")
           (provision '(networking))
           (requirement '(user-processes dbus-system loopback))
           (start #~(make-forkexec-constructor
                     (list (string-append #$pkg "/libexec/iwd"))
                     #:log-file "/var/log/iwd.log"))
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

(define-public iwd-service-type
  (let ((iwd-package (compose list iwd-configuration-package)))
    (service-type (name 'iwd)
                  (extensions
                   (list (service-extension shepherd-root-service-type
                                            iwd-shepherd-service)
                         (service-extension dbus-root-service-type
                                            iwd-package)
                         (service-extension etc-service-type
                                            iwd-etc-service)
                         (service-extension profile-service-type
                                            iwd-package)))
                  (default-value (iwd-configuration))
                  (description
                   "Run @url{https://01.org/iwd,iwd},
a wpa-supplicant replacemennt."))))

;; (define-record-type* <doas-configuration>
;;   doas-configuration make-doas-configuration
;;   doas-configuration?
;;   (doas doas-configuration-doas (default doas))
;;   (rules doas-configuration-rules
;;          (default '("permit :wheel"))))

;; permit persist setenv { PKG_CACHE PKG_PATH } aja cmd pkg_add
;; permit setenv { -ENV PS1=$DOAS_PS1 SSH_AUTH_SOCK } :wheel
;; permit nopass tedu as root cmd /usr/sbin/procmap
;; permit nopass keepenv setenv { PATH } root as root
