(define-module (home yggdrasil ssh)
  #:use-module (gnu packages ssh)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:use-module (gnu home-services ssh))

(define-public services
  (list
   (service
    home-ssh-service-type
    (home-ssh-configuration
     (package openssh-sans-x)
     (user-known-hosts-file
      '("~/.dotfiles/home/yggdrasil/files/config/ssh/known_hosts"
        "~/.ssh/my_known_hosts"))
     (default-host "*")
     (default-options
       '((address-family . "inet")))
     (extra-config
      (list
       (ssh-host
        (host "my_git")
        (options
         `((hostname . ,(getenv "URI_ssh_my_git"))
           (identity-file . ,(string-append
                              "~/.ssh/" (getenv "KEY_ssh_my_git")))
           (port . ,(getenv "PORT_ssh_my_git"))
           (user . ,(getenv "ID_ssh_my_git")))))
       (ssh-host
        (host "my_server")
        (options
         `((hostname . ,(getenv "URI_ssh_my_server"))
           (identity-file . ,(string-append
                              "~/.ssh/" (getenv "KEY_ssh_my_server")))
           (port . ,(getenv "PORT_ssh_my_server"))
           (user . ,(getenv "ID_ssh_my_server")))))
       (ssh-host
        (host "pre_site")
        (options
         `((hostname . ,(getenv "URI_ssh_pre_site"))
           (identity-file . ,(string-append
                              "~/.ssh/" (getenv "KEY_ssh_pre_site")))
           (port . ,(getenv "PORT_ssh_pre_site"))
           (user . ,(getenv "ID_ssh_pre_site")))))
       (ssh-host
        (host "pre_bitwarden")
        (options
         `((hostname . ,(getenv "URI_ssh_pre_bitwarden"))
           (identity-file . ,(string-append
                              "~/.ssh/" (getenv "KEY_ssh_pre_bitwarden")))
           (port . ,(getenv "PORT_ssh_pre_bitwarden"))
           (user . ,(getenv "ID_ssh_pre_bitwarden")))))))))))

(define-public known-hosts-config
  (list
   `("ssh/my_known_hosts"
     ,(plain-file
       "my_known_hosts"
       (string-append (getenv "URI_ssh_pre_bitwarden") " " (getenv "HOSTKEY_ssh_pre_bitwarden") "\n"
                      (getenv "URI_ssh_pre_site") " " (getenv "HOSTKEY_ssh_pre_site") "\n"
                      (getenv "URI_ssh_my_git") " " (getenv "HOSTKEY_ssh_my_git") "\n"
                      (getenv "URI_ssh_my_server") " " (getenv "HOSTKEY_ssh_my_server"))))))
