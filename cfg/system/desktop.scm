(define-module (system desktop)
  #:use-module (guix gexp)
  #:use-module (guix transformations)

  #:use-module (gnu system)
  #:use-module (gnu system keyboard)

  #:use-module (gnu packages)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages display-managers)

  #:use-module (gnu services)
  #:use-module (gnu services desktop)
  #:use-module (gnu services dbus)
  #:use-module ((gnu services networking) #:select (openntpd-service-type
                                                    openntpd-configuration))
  #:use-module (gnu services linux)
  #:use-module (gnu services xorg)
  #:use-module (gnu services cups)
  #:use-module (gnu services sddm)

  #:use-module (utils)
  #:use-module ((system base) :prefix base:)
  #:use-module ((services) #:select (iwd-service-type
                                     connman-service-type)))


(define libinput-config
  #,(ml "Section \"InputClass\""
        "Identifier \"libinput touchpad catchall\""
        "MatchIsTouchpad \"on\""
        "Driver \"libinput\""
        "Option \"Tapping\" \"on\""
        "Option \"TappingDrag\" \"on\""
        "EndSection"))


(define xorg-layout
  (keyboard-layout
   "us,ru"
   #:options '("grp:toggle" "ctrl:swapcaps")))


(define-public services
  (cons*
   polkit-wheel-service
   fontconfig-file-system-service
   (service polkit-service-type)
   (service iwd-service-type)
   (service connman-service-type)
   (service elogind-service-type
            (elogind-configuration
             (handle-lid-switch 'suspend)
             (handle-lid-switch-external-power 'suspend)
             (handle-lid-switch-docked 'ignore)))
   (service bluetooth-service-type)
   (service openntpd-service-type
            (openntpd-configuration
             (sensor '("*"))
             (constraint-from '("www.gnu.org"))))
   (service cups-service-type
            (cups-configuration
             (extensions (list splix cups-filters))
             (default-paper-size "A4")
             (web-interface? #t)))
   (service sane-service-type)
   (service sddm-service-type
            (sddm-configuration
             (display-server "wayland")
             (theme "guix-simplyblack-sddm")
             (themes-directory
              #~(string-append #$guix-simplyblack-sddm-theme
                               "/share/sddm/themes"))
             (xorg-configuration
              (xorg-configuration
               (modules (list xf86-video-intel
                              xf86-input-libinput
                              xf86-video-vesa))
               (keyboard-layout xorg-layout)
               (extra-config (list libinput-config))))))
   base:services))


(define transform-pkgs
  (options->transformation
   '((with-commit . "swappy=v1.3.1"))))


(define-public packages
  (append
   base:packages
   (map (compose transform-pkgs specification->package)
        '("swappy"))
   (map specification->package
        '(;; wayland
          "sway" "wofi" "bemenu" "mako" "i3status" "grim" "slurp"
          "wl-clipboard" "xdg-desktop-portal" "xdg-desktop-portal-wlr"

          ;; themes
          "moka-icon-theme" "arc-icon-theme" "orchis-theme"

          ;; fonts
          "fontconfig" "font-iosevka" "font-openmoji" "font-awesome"
          "font-google-roboto"

          ;; stuff
          "alacritty" "pulseaudio" "pavucontrol" "bluez"
          "telegram-desktop" "ntfs-3g" "intel-vaapi-driver" "dbus"
          "flatpak"
          ))))


(define-public system base:system)
