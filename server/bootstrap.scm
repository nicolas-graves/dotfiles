(use-modules (gnu))
(use-service-modules networking ssh)
(use-package-modules bootloaders ssh)

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
  (services
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
                    (string-append "../keys/" (getenv "KEY_ssh_my_server") ".pub"))))))))
           (modify-services %base-services
             (guix-service-type
              config => (guix-configuration
                         (inherit config)
                         (authorized-keys
                          (append (list (local-file "../keys/ldlc-signing-key.pub")
                                        (local-file "../keys/dell-signing-key.pub"))
                           %default-authorized-guix-keys))))))))
