(define-module (mnp)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (srfi srfi-26)
  #:use-module (guix store)
  #:use-module ((default) #:prefix default:)
  #:use-module ((udev) #:prefix udev:)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages cups)
  #:use-module (nongnu packages linux)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu services linux)
  #:use-module (gnu services nix)
  #:use-module (gnu services xorg)
  #:use-module (gnu services pm)
  #:use-module (gnu services sound)
  #:use-module (gnu services audio)
  #:use-module (gnu services cups)
  #:use-module (gnu services dbus)
  #:use-module (gnu services sddm)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services docker)
  #:use-module (nongnu system linux-initrd)
  #:use-module (guix gexp))

(define users
  (cons*
   (user-account
    ;;(shell #~(string-append #$zsh "/bin/zsh"))
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
  (cons* (file-system (device (uuid "54da3a9c-7c46-416b-9065-48199d8e7536"))
                      (mount-point "/")
                      (type "btrfs")
                      (dependencies luks-mapped-devices))
         (file-system (device (uuid "D2A9-6614" 'fat))
                      (mount-point "/boot/efi")
                      (type "vfat"))
         %base-file-systems))

(define bbswitch-config
  (plain-file "bbswitch.conf"
              "options bbswitch load_state=0 unload_state=1"))

(define libinput-config
  (string-join
   '("Section \"InputClass\""
     "Identifier \"libinput touchpad catchall\""
     "MatchIsTouchpad \"on\""
     "Driver \"libinput\""
     "Option \"Tapping\" \"on\""
     "Option \"TappingDrag\" \"on\""
     "EndSection")
   "\n"))

(define xorg-layout
  (keyboard-layout "us,ru" #:options '("grp:toggle")))

(define services
  (cons*
   (dbus-service)
   (polkit-service)
   (elogind-service)
   (bluetooth-service #:auto-enable? #t)
   ;; (screen-locker-service xscreensaver "slock")
   (service wpa-supplicant-service-type)
   (service nix-service-type)
   (service kernel-module-loader-service-type '("bbswitch"))
   (simple-service bbswitch-config
                   etc-service-type
                   (list `("modprobe.d/bbswitch.conf"
                           ,bbswitch-config)))
   (service network-manager-service-type)
   (service docker-service-type)
   (service openntpd-service-type
            (openntpd-configuration
             (allow-large-adjustment? #t)))
   (service cups-service-type
            (cups-configuration
             (extensions (list splix cups-filters))
             (default-paper-size "A4")
             (web-interface? #t)))
   (service tlp-service-type
            (tlp-configuration
             (sata-linkpwr-on-bat "max_performance")))
   (service alsa-service-type)
   (service pulseaudio-service-type
            (pulseaudio-configuration
             (script-file (local-file "/home/kreved/.config/pulse/default.pa"))
             (system-script-file (local-file "/home/kreved/.config/pulse/system.pa"))))
   (service sddm-service-type
            (sddm-configuration
             (theme "maldives")
             (xorg-configuration (xorg-configuration
                                  (modules (list xf86-video-intel
                                                 xf86-input-libinput
                                                 xf86-video-vesa))
                                  (keyboard-layout xorg-layout)
                                  (extra-config (list libinput-config))))))
   (modify-services %base-services
     (guix-service-type config =>
                        (guix-configuration
                         (inherit config)
                         (substitute-urls (cons*
                                           "https://mirror.brielmaier.net"
                                           %default-substitute-urls))
                         (authorized-keys (cons*
                                           (local-file "mirror.brielmaier.net.pub")
                                           %default-authorized-guix-keys))))
     (udev-service-type config =>
                        (udev-configuration
                         (inherit config)
                         (rules (cons* udev:st-link-rule
                                       udev:caterina-rule
                                       (udev-configuration-rules config))))))))


(define packages
  (append
   (map (cut specification->package <>)
        '("ghc@8.6.5"
          "gcc-toolchain"
          "xmonad"
          "ghc-xmonad-contrib"
          "xmobar"
          "ntfs-3g"
          "intel-vaapi-driver"))
   default:base-packages))

(operating-system
 (inherit default:base-system)
 (initrd microcode-initrd)
 (host-name "asgard")
 (kernel linux)
 (firmware (list linux-firmware))
 (kernel-loadable-modules (list bbswitch-module))
 (kernel-arguments '("modprobe.blacklist=nouveau"))
 (swap-devices '("/var/swapfile"))
 (mapped-devices luks-mapped-devices)
 (file-systems file-systems)
 (users users)
 (packages packages)
 (services services))


; sudo -E guix system -L ~/.config/guix/system/ reconfigure ~/.config/guix/system/mnp.scm
