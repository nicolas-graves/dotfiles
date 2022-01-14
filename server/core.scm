(use-modules (guix gexp)
             (gnu system)
             (gnu machine)
             (gnu machine ssh)
             (gnu system accounts)
             (gnu system shadow)
             (gnu packages version-control)
             ((server base) :prefix base:)
             ((server git) :prefix git:)
             ((server cuirass) :prefix cuirass:)
             ((server rsync) :prefix rsync:)
             ((server packages) :prefix packages:))

;; If needed, add a cuirass package here.
(define %server
  (operating-system
    (inherit base:server)
    (users (append (list git:user) %base-user-accounts))
    (services (append cuirass:services git:services rsync:services))
    (packages (append packages:packages %base-packages))))

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
