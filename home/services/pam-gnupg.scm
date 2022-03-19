(define-module (home services pam-gnupg)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services avahi)
  #:use-module (gnu services xorg)
  #:use-module (gnu services networking)
  #:use-module (gnu services sound)
  #:use-module ((gnu system file-systems)
                #:select (%elogind-file-systems file-system))
  #:use-module (gnu system)
  #:use-module (gnu system setuid)
  #:use-module (gnu system shadow)
  #:use-module (gnu system pam)
  #:use-module (gnu packages linux)
  #:use-module (guix deprecation)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (pam-gnupg-configuration
            pam-gnupg-configuration?
            pam-gnupg-service-type))


(define-record-type* <pam-gnupg-configuration> pam-gnupg-configuration
  make-pam-gnupg-configuration
  pam-gnupg-configuration?
  (keyring pam-gnupg-package (default pam-gnupg))
  (pam-services pam-gnupg-pam-services (default '(("login" . login)
                                                  ("passwd" . passwd)))))
(define (pam-gnupg config)
  (define (%pam-keyring-entry . arguments)
    (pam-entry
     (control "optional")
     (module (file-append (pam-gnupg-package config)
                          "/lib/security/pam_gnupg.so"))
     (arguments arguments)))

  (list
   (lambda (service)
     (case (assoc-ref (pam-gnupg-pam-services config)
                      (pam-service-name service))
       ((login)
         (display service)
         (display "\n")
        (pam-service
         (inherit service)
         (auth (append (pam-service-auth service)
                       (list (%pam-keyring-entry "store-only"))))
         (session (append (pam-service-session service)
                          (list (%pam-keyring-entry))))))
       ((passwd)
         (display service)
         (display "\n")
        (pam-service
         (inherit service)
         (password (append (pam-service-password service)
                           (list (%pam-keyring-entry))))))
       (else service)))))

(define pam-gnupg-service-type
  (service-type
   (name 'pam-gnupg)
   (extensions (list
                (service-extension pam-root-service-type pam-gnupg)))
   (default-value (pam-gnupg-configuration))
   (description "Return a service, that adds the @code{pam-gnupg} package
to the system profile and extends PAM with entries using
@code{pam_gnupg.so}, unlocking a user's GnuPG's key when they log in
or setting its password with passwd.")))
