(define-module (system yggdrasil)
  #:use-module (guix gexp)
  #:use-module (guix store)

  #:use-module (gnu system)
  #:use-module (gnu system shadow)
  #:use-module (gnu system file-systems)

  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)

  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages shells)

  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services nix)
  #:use-module (gnu services linux)
  #:use-module (gnu services docker)
  #:use-module (gnu services pm)
  #:use-module (gnu services xorg)

  #:use-module (nongnu system linux-initrd)
  #:use-module (nongnu packages linux)

  #:use-module ((system desktop) #:prefix desktop:))


(define users
  (cons*
   (user-account
    (name "graves")
    (group "users")
    (supplementary-groups '("wheel" "netdev" "tty" "input" "realtime"  "audio" "video" "docker" "lp"))
    (home-directory "/home/graves")
    (shell (file-append zsh "/bin/zsh")))
   %base-user-accounts))


(define file-systems
  (cons* (file-system
           (device "/dev/sda3")
           (mount-point "/")
           (type "ext4"))
         (file-system
           (device "/dev/sda4")
           (mount-point "/home")
           (type "ext4"))
         (file-system
           (device "/dev/sda1")
           (mount-point "/boot/efi")
           (type "vfat"))
         %base-file-systems))


(define services
  (cons*
   (service nix-service-type)
   (service docker-service-type)
   (service tlp-service-type
	    (tlp-configuration
	      (cpu-boost-on-ac? #t)
	      (wifi-pwr-on-bat? #t)))
   (service screen-locker-service-type
            (screen-locker "swaylock"
                           (file-append swaylock "/bin/swaylock")
                           #f))
   (modify-services desktop:services
     (guix-service-type
      config =>
      (guix-configuration
       (inherit config)
       (authorized-keys (cons*
                         (local-file "../keys/nonguix.pub")
                         %default-authorized-guix-keys)))))))


(define packages
  (append
   desktop:packages
   (map specification->package
        '("curl"
          "htop"
          "swaylock"))))


(operating-system
  (inherit desktop:system)
  (initrd microcode-initrd)
  (host-name "graves")
  (kernel linux)
  (firmware (list linux-firmware))
  (swap-devices '("/dev/sda2"))
  (file-systems file-systems)
  (users users)
  (groups desktop:groups)
  (packages packages)
  (services services)
  (pam-services desktop:pam-services))
