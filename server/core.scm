(use-modules (gnu machine)
             (gnu machine ssh)
             ((server base) :prefix base:))

(list (machine
       (operating-system base:server)
       (environment managed-host-environment-type)
       (configuration
        (machine-ssh-configuration
         (host-name (getenv "URI_ssh_my_server"))
         (host-key (getenv "HOSTKEY_ssh_my_server"))
         (system "x86_64-linux")
         (user (getenv "ID_ssh_my_server"))
         (identity (string-append "~/.ssh/" (getenv "KEY_ssh_my_server")))
         (port (string->number (getenv "PORT_ssh_my_server")))))))
