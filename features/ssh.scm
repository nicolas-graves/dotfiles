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
          (ssh-agent? #f))
  "Setup and configure ssh and ssh-agent."
  (ensure-pred home-ssh-configuration? ssh-configuration)
  (ensure-pred boolean? ssh-agent?)

  (define (ssh-home-services config)
    "Returns home services related to SSH."
    (append
     (if ssh-agent?
         (let* ((runtime (or (getenv "XDG_RUNTIME_DIR")
                             (format #f "/run/user/~a" (getuid))))
                (sock (string-append runtime "/ssh-agent.sock")))
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
                        (list (string-append
                                 #$(get-value 'ssh config)
                                 "/bin/ssh-agent")
                              "-d" "-a" #$sock)
                        #:log-file (string-append
                                    (or (getenv "HOME")
                                        (format #f "/home/~a" (getlogin)))
                                    "/.local/var/log/ssh-agent.log")))
                    (stop #~(make-kill-destructor)))))
            (simple-service
             'ssh-auth-socket-env-export
             home-environment-variables-service-type
             `(("SSH_AUTH_SOCK" . ,sock))))))
     (list (service home-ssh-service-type
                    ssh-configuration))))

  (feature
   (name 'ssh)
   (values `((ssh . ,openssh)))
   (home-services-getter ssh-home-services)))
