(define-module (system yggdrasil)
  #:use-module (guix gexp)
  #:use-module (guix store)

  #:use-module (gnu system)
  #:use-module (gnu system shadow)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system file-systems)

  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)

  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages wm)

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
    (name "kreved")
    (group "users")
    (supplementary-groups '("wheel" "audio" "video" "docker" "lp"))
    (home-directory "/home/kreved"))
   %base-user-accounts))


(define luks-mapped-devices
  (list (mapped-device
          (source (uuid "18a3881f-1573-44d0-918c-1fe872224954"))
          (target "guix")
          (type luks-device-mapping))))


(define file-systems
  (cons* (file-system
           (device (uuid "394d0a12-9e8b-4af2-8242-8be814dadbdf"))
           (mount-point "/")
           (type "ext4")
           (dependencies luks-mapped-devices))
         (file-system
           (device (uuid "F8A4-5BF5" 'fat))
           (mount-point "/boot")
           (type "vfat"))
         %base-file-systems))


(define services
  (cons*
   (service nix-service-type)
   (service docker-service-type)
   (service tlp-service-type)
   (service screen-locker-service-type
            (screen-locker "swaylock"
                           (file-append swaylock "/bin/swaylock")
                           #f))
   (modify-services desktop:services
     (guix-service-type
      config =>
      (guix-configuration
       (inherit config)
       (substitute-urls (cons*
                         "https://mirror.brielmaier.net"
                         %default-substitute-urls))
       (authorized-keys (cons*
                         (local-file "../keys/nonguix.pub")
                         %default-authorized-guix-keys)))))))


(define packages
  (append
   desktop:packages
   (map specification->package '("direnv"
                                 "curl"
                                 "htop"
                                 "ripgrep"
                                 "docker-cli"
                                 "docker-compose"
                                 "containerd"
                                 "nix"
                                 "swaylock"))))


(operating-system
  (inherit desktop:system)
  (initrd microcode-initrd)
  (host-name "yggdrasil")
  (kernel linux)
  (firmware (list ibt-hw-firmware iwlwifi-firmware))
  (swap-devices '("/var/swapfile"))
  (mapped-devices luks-mapped-devices)
  (file-systems file-systems)
  (users users)
  (packages packages)
  (services services))
