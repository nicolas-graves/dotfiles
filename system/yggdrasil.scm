(define-module (yggdrasil)
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

  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services nix)
  #:use-module (gnu services linux)
  #:use-module (gnu services docker)
  #:use-module (gnu services pm)

  #:use-module (nongnu system linux-initrd)
  #:use-module (nongnu packages linux)

  #:use-module ((desktop) #:prefix desktop:)
  #:use-module ((udev) #:prefix udev:)
  #:use-module (packages))


(define users
  (cons*
   (user-account
    (name "kreved")
    (group "users")
    (supplementary-groups '("wheel" "audio" "video" "docker"))
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
                         %default-authorized-guix-keys))))
     (udev-service-type
      config =>
      (udev-configuration
       (inherit config)
       (rules (cons* udev:st-link-rule
                     udev:caterina-rule
                     (udev-configuration-rules config))))))))


(define packages
  (append
   (map specification->package
        '("direnv" "curl" "htop" "make" "ripgrep"
          "gnupg" "docker-cli" "docker-compose" "nix"
          "node" "openjdk@11.28"))
   desktop:packages))


(operating-system
  (inherit desktop:system)
  (initrd microcode-initrd)
  (host-name "yggdrasil")
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


;; sudo -E guix system -L ~/.config/guix/system/ reconfigure ~/.config/guix/system/yggdrasil.scm
;; for profile in $GUIX_EXTRA_PROFILES/*; do guix package --profile="$profile/$(basename $profile)" --manifest="$HOME/.config/guix/manifests/$(basename $profile).scm"; done
