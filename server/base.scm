(define-module (server base)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (guix gexp)

  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)

  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)

  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)

  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages ssh))

(define-public services
  (append (list
	   (service dhcp-client-service-type)
           (service
            openssh-service-type
            (openssh-configuration
             (openssh openssh-sans-x)
             (permit-root-login #t)
             (authorized-keys
              `((,(getenv "ID_ssh_my_server")
                 ,(local-file
                   (string-append "../keys/" (getenv "KEY_ssh_my_server") ".pub")))))))
           (extra-special-file
            (string-append "/" (getenv "ID_ssh_my_server") "/.dotfiles/keys/"
                           (getenv "KEY_ssh_my_server") ".pub")
            (local-file "../keys/id_rsa_git.pub"))
           (extra-special-file
            (string-append "/" (getenv "ID_ssh_my_server")
                           "/.dotfiles/keys/ldlc-signing-key.pub")
            (local-file "../keys/ldlc-signing-key.pub"))
           (extra-special-file
            (string-append "/" (getenv "ID_ssh_my_server")
                           "/.dotfiles/keys/dell-signing-key.pub")
            (local-file "../keys/dell-signing-key.pub")))
          (modify-services %base-services
            (guix-service-type
             config => (guix-configuration
                        (inherit config)
                        (authorized-keys
                         (append (list (local-file "../keys/ldlc-signing-key.pub")
                                       (local-file "../keys/dell-signing-key.pub"))
                                 %default-authorized-guix-keys)))))))

(define-public server
  (operating-system
    (host-name "gnu")
    (timezone "Etc/UTC")
    (file-systems (cons* (file-system
                           (mount-point "/")
                           (device "/dev/vda2")
                           (type "ext4"))
                         %base-file-systems))
    (keyboard-layout (keyboard-layout "fr"))
    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (targets '("/dev/vda"))
	         (terminal-outputs '(console))
	         (keyboard-layout keyboard-layout)))
    (services base-services)))
