(define-module (system asgard)
  #:use-module (srfi srfi-10)

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
         (source (uuid "a97291e3-d230-4c50-8387-bf0774684395"))
         (target "guix")
         (type luks-device-mapping))))


(define file-systems
  (cons* (file-system
           (device (uuid "54da3a9c-7c46-416b-9065-48199d8e7536"))
           (mount-point "/")
           (type "btrfs")
           (dependencies luks-mapped-devices))
         (file-system
           (device (uuid "D2A9-6614" 'fat))
           (mount-point "/boot/efi")
           (type "vfat"))
         %base-file-systems))


(define bbswitch-config
  (plain-file "bbswitch.conf"
              "options bbswitch load_state=0 unload_state=1"))


(define services
  (cons*
   (service nix-service-type)
   (service kernel-module-loader-service-type '("bbswitch"))
   (simple-service 'bbswitch-conf
                   etc-service-type
                   `(("modprobe.d/bbswitch.conf" ,bbswitch-config)))
   (service docker-service-type)
   (service tlp-service-type
            (tlp-configuration
             (sata-linkpwr-on-bat "max_performance")))
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
  (host-name "asgard")
  (kernel linux)
  (firmware (list ibt-hw-firmware iwlwifi-firmware))
  (kernel-loadable-modules (list bbswitch-module
                                 v4l2loopback-linux-module))
  (kernel-arguments '("modprobe.blacklist=nouveau"))
  (swap-devices '("/var/swapfile"))
  (mapped-devices luks-mapped-devices)
  (file-systems file-systems)
  (users users)
  (packages packages)
  (services services))


;; sudo -E guix system -L ~/.config/guix/cfg/ reconfigure ~/.config/guix/cfg/system/asgard.scm
