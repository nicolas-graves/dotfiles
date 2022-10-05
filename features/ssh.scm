(define-module (features ssh)
  #:use-module (guix gexp)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde packages)
  #:use-module (gnu home-services ssh)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services security-token)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
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
          (smart-card? #f)
          (ssh-ssh-agent? #f))
  "Setup and configure SSH, ssh-agent, and take into account smart cards."
  (ensure-pred home-ssh-configuration? ssh-configuration)
  (ensure-pred boolean? smart-card?)
  (ensure-pred boolean? ssh-ssh-agent?)

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
    (append
     (if ssh-ssh-agent?
         (list
          (simple-service
           'start-ssh-agent-at-startup
           home-shepherd-service-type
           (list (shepherd-service
                  (documentation "Run the ssh-agent at startup.")
                  (provision '(ssh-agent))
                  (requirement '())
                  (start
                   #~(make-forkexec-constructor
                      (list #$(file-append
                               (get-value 'ssh config)
                               "/bin/ssh-agent")
                            "-a" "/tmp/ssh-agent.socket" "-D")))
                  (stop #~(make-kill-destructor)))))
          (simple-service
           'ssh-auth-export
           home-environment-variables-service-type
           `(("SSH_AUTH_SOCK" . "/tmp/ssh-agent.socket")))))
     (list (service home-ssh-service-type
		    ssh-configuration))))

  (feature
   (name 'ssh)
   (values `((ssh . ,openssh)
             (smart-card? . smart-card?)))
   (home-services-getter ssh-home-services)
   (system-services-getter ssh-system-services)))
