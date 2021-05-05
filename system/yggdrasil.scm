(define-module (yggdrasil)
  #:use-module (srfi srfi-10)
  #:use-module (guix store)
  #:use-module (gnu)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages display-managers)
  #:use-module (gnu services desktop)
  #:use-module (gnu services dbus)
  #:use-module (gnu services networking)
  #:use-module (gnu services linux)
  #:use-module (gnu services nix)
  #:use-module (gnu services xorg)
  #:use-module (gnu services pm)
  #:use-module (gnu services sound)
  #:use-module (gnu services audio)
  #:use-module (gnu services cups)
  #:use-module (gnu services sddm)
  #:use-module (gnu services xorg)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services docker)
  #:use-module (gnu services databases)
  #:use-module (nongnu system linux-initrd)
  #:use-module (nongnu packages linux)
  #:use-module ((default) #:prefix default:)
  #:use-module ((udev) #:prefix udev:)
  #:use-module (packages)
  #:use-module ((services) #:prefix services:))


(define-reader-ctor 'ml
  (λ strs
    (string-join strs "\n")))


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
  #,(ml "Section \"InputClass\""
        "Identifier \"libinput touchpad catchall\""
        "MatchIsTouchpad \"on\""
        "Driver \"libinput\""
        "Option \"Tapping\" \"on\""
        "Option \"TappingDrag\" \"on\""
        "EndSection"))


(define xorg-layout
  (keyboard-layout "us,ru" #:options '("grp:toggle")))


(define services
  (cons*
   (dbus-service)
   (polkit-service)
   polkit-wheel-service
   fontconfig-file-system-service
   (service services:iwd-service-type)
   (service services:connman-service-type
            (services:connman-configuration
             (connman connman-with-iwd)
             (wifi-agent 'iwd)))
   (elogind-service
    #:config (elogind-configuration
              (handle-lid-switch 'suspend)
              (handle-lid-switch-external-power 'suspend)
              (handle-lid-switch-docked 'suspend)))
   (bluetooth-service #:auto-enable? #f)
   #;(service wpa-supplicant-service-type)
   (service nix-service-type)
   (service kernel-module-loader-service-type '("bbswitch"))
   (simple-service 'bbswitch-conf
                   etc-service-type
                   (list `("modprobe.d/bbswitch.conf" ,bbswitch-config)))
   #;(service network-manager-service-type)
   (service docker-service-type)
   (service openntpd-service-type)
   (service cups-service-type
            (cups-configuration
             (extensions (list splix cups-filters))
             (default-paper-size "A4")
             (web-interface? #t)))
   (service sane-service-type)
   (service tlp-service-type
            (tlp-configuration
             (sata-linkpwr-on-bat "max_performance")))
   (service alsa-service-type)
   (service postgresql-service-type
            (postgresql-configuration
             (postgresql postgresql-13.2)
             (port 7654)
             (config-file
              (postgresql-config-file (socket-directory #f)))))
   (service sddm-service-type
            (sddm-configuration
             (display-server "wayland")
             (theme "guix-simplyblack-sddm")
             (themes-directory #~(string-append #$guix-simplyblack-sddm-theme "/share/sddm/themes"))
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
                                           (local-file "../keys/nonguix.pub")
                                           %default-authorized-guix-keys))))
     (udev-service-type config =>
                        (udev-configuration
                         (inherit config)
                         (rules (cons* udev:st-link-rule
                                       udev:caterina-rule
                                       (udev-configuration-rules config))))))))


(define packages
  (append
   (map specification->package
        '("ghc@8.6.5" "gcc-toolchain" "xmonad" "ghc-xmonad-contrib" "xmobar"
          "xclip" "dmenu" "sxiv" "feh" "picom" "maim" "xrandr"
          "ntfs-3g" "dbus"
          "intel-vaapi-driver"
          "sway" "wofi" "bemenu" "mako" "i3status" "grim" "slurp" "wl-clipboard"
          "pipewire@0.3.22" "xdg-desktop-portal" "xdg-desktop-portal-wlr"
          "alacritty" "alsa-utils" "pulseaudio" "pavucontrol" "bluez" "telegram-desktop"
          "direnv" "curl" "htop" "make" "openssh" "ripgrep"
          "gnupg"
          "docker-cli" "docker-compose"
          "nix" "node" "openjdk@11.28"
          "fontconfig" "font-iosevka" "font-iosevka-aile" "font-gnu-unifont"))
   default:base-packages))


(operating-system
  (inherit default:base-system)
  (initrd microcode-initrd)
  (host-name "yggdrasil")
  (kernel linux)
  (firmware (list ibt-hw-firmware iwlwifi-firmware))
  (kernel-loadable-modules (list bbswitch-module v4l2loopback-linux-module))
  (kernel-arguments '("modprobe.blacklist=nouveau"))
  (swap-devices '("/var/swapfile"))
  (mapped-devices luks-mapped-devices)
  (file-systems file-systems)
  (users users)
  (packages packages)
  (services services))


;; sudo -E guix system -L ~/.config/guix/system/ reconfigure ~/.config/guix/system/yggdrasil.scm
;; for profile in $GUIX_EXTRA_PROFILES/*; do guix package --profile="$profile/$(basename $profile)" --manifest="$HOME/.config/guix/manifests/$(basename $profile).scm"; done
