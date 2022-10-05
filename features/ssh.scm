(define-module (features ssh)
  #:use-module (guix gexp)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu home-services ssh)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services security-token)
  #:use-module (gnu packages)
  #:use-module (gnu packages ssh)
  #:use-module (packages cryptography)

  #:export (feature-ssh)

  #:re-export (home-ssh-configuration
	       ssh-host
	       ssh-match))


(define* (feature-ssh
	  #:key
          (ssh openssh)
	  (ssh-configuration (home-ssh-configuration))
          (smart-card? #f))
  "Setup and configure SSH, ssh-agent, and take into account smart cards."
  (ensure-pred home-ssh-configuration? ssh-configuration)
  (ensure-pred boolean? smart-card?)

  (define (ssh-system-services config)
    (if smart-card?
        (list
         (service pcscd-service-type)
         (udev-rules-service
          'yubikey
          (file->udev-rule
           "70-u2f.rules"
           (file-append libfido2 "/udev/rules.d/70-u2f.rules"))
          #:groups '("plugdev")))
        '()))

  (define (ssh-home-services config)
    "Returns home services related to SSH."
    (list (service home-ssh-service-type
		   ssh-configuration)))

  (feature
   (name 'ssh)
   (values `((ssh . ,openssh)
             (smart-card? . smart-card?)))
   (home-services-getter ssh-home-services)
   (system-services-getter ssh-system-services)))
