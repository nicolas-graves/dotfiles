(define-module (home yggdrasil shell)
  #:use-module (gnu services)
  #:use-module (gnu home-services shells)
  #:use-module (gnu home-services shellutils))

(define-public services
  (list
   (service home-bash-service-type
            (home-bash-configuration
             (bash-profile
              '("source /run/current-system/profile/etc/profile.d/nix.sh"))
             (environment-variables
              '(("XDG_CURRENT_DESKTOP" . "sway")
                ("XDG_SESSION_TYPE" . "wayland")
                ("QT_QPA_PLATFORM" . "wayland-egl")))))
   (service home-bash-direnv-service-type)))
