(define-module (system desktop)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)

  #:use-module (gnu system)
  #:use-module (gnu system keyboard)

  #:use-module (gnu packages)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages display-managers)

  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services dbus)
  #:use-module (gnu services networking)
  #:use-module (gnu services linux)
  #:use-module (gnu services xorg)
  #:use-module (gnu services cups)
  #:use-module (gnu services sddm)

  #:use-module ((system base) :prefix base:)
  #:use-module (services))


(define-public services
  (cons*
   polkit-wheel-service
   (service polkit-service-type)
   (service iwd-service-type
            (iwd-configuration
             (config
              '((General
                 ((EnableNetworkConfiguration . true)))
                (Network
                 ((NameResolvingService . resolvconf)))))))
   ;; (service earlyoom-service-type)
   (service elogind-service-type
            (elogind-configuration
             (handle-lid-switch 'suspend)
             (handle-lid-switch-external-power 'suspend)
             (handle-lid-switch-docked 'ignore)))
   (service bluetooth-service-type
            (bluetooth-configuration
             (auto-enable? #t)))
   (service ntp-service-type)
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
                               "/share/sddm/themes"))))
   (modify-services base:services
     (udev-service-type
      config =>
      (udev-configuration
       (inherit config)
       (rules (cons*
               light
               (file->udev-rule
                "70-u2f.rules"
                (origin
                  (method url-fetch)
                  (uri "https://raw.githubusercontent.com/Yubico/libfido2/master/udev/70-u2f.rules")
                  (sha256
                   (base32 "1dkfqb7sfj92zvckfpnykwrd4a52fasgkziznahm54izjnb71gii"))))
               (udev-configuration-rules config))))))))

(define-public packages
  (append
   base:packages
   (map specification->package
        '("ntfs-3g"
          ;; "sway@1.5.1"
          "qtwayland"
          "intel-vaapi-driver"
          "libva-utils"))))


(define-public system base:system)
