(use-modules (guix gexp)
             (gnu system)
             (gnu machine)
             (gnu machine ssh)
             (gnu packages)
             (gnu system accounts)
             (gnu system shadow)
             (gnu packages version-control)
             ((server base) :prefix base:)
             ((server git) :prefix git:)
             ((server cuirass) :prefix cuirass:)
             ((server rsync) :prefix rsync:)
             ((server mail) :prefix mail:))

(define %packages
  (map (compose list specification->package+output)
       (append '("htop")
               git:packages
               cuirass:packages
               rsync:packages
               mail:packages)))

(define %services
  (append
   cuirass:services
   git:services
   rsync:services
   mail:services))

;; If needed, add a cuirass package here.
(define %server
  (operating-system
    (inherit base:server)
    (users (append (list git:user) %base-user-accounts))
    (services %services)
    (packages (append %packages %base-packages))))

(list (machine
       (operating-system %server)
       (environment managed-host-environment-type)
       (configuration
        (machine-ssh-configuration
         (host-name (getenv "URI_ssh_my_server"))
         (host-key (getenv "HOSTKEY_ssh_my_server"))
         (system "x86_64-linux")
         (user (getenv "ID_ssh_my_server"))
         (identity (string-append "~/.ssh/" (getenv "KEY_ssh_my_server")))
         (port (string->number (getenv "PORT_ssh_my_server")))))))
