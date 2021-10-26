(define-module (home yggdrasil shell)
  #:use-module (gnu services)
  #:use-module (gnu home-services shells)
  #:use-module (gnu home-services shellutils))

(define (wrap str)
  (string-append "\"" str "\""))

(define-public services
  (list
   (service home-bash-service-type
            (home-bash-configuration
             (bash-profile
              '("source /run/current-system/profile/etc/profile.d/nix.sh"))
             (environment-variables
              `(("XDG_CURRENT_DESKTOP" . "sway")
                ("XDG_SESSION_TYPE" . "wayland")
                ("MOZ_ENABLE_WAYLAND" . "1")
                ("BEMENU_OPTS" . ,(wrap
                                   (string-join '("--fn 'Iosevka Light 18'"
                                                  "--nb '#FFFFFF'"
                                                  "--nf '#000000'"
                                                  "--tb '#FFFFFF'"
                                                  "--tf '#000000'"
                                                  "--fb '#FFFFFF'"
                                                  "--ff '#000000'"
                                                  "--hb '#F0F0F0'"
                                                  "--hf '#721045'")
                                                " ")))))))
   (service home-bash-direnv-service-type)))
