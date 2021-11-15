(define-module (home yggdrasil shell)
  #:use-module (guix gexp)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu services)
  #:use-module (gnu home-services shells)
  #:use-module (gnu home-services shellutils))

(define (wrap str)
  (string-append "\"" str "\""))

(define-public services
  (list
   (simple-service
    'set-wayland-vars
    home-environment-variables-service-type
    `(("XDG_CURRENT_DESKTOP" . "sway")
      ("XDG_SESSION_TYPE" . "wayland")
      ("QT_QPA_PLATFORM" . "wayland-egl")
      ("BEMENU_OPTS" . ,(wrap
                         (string-join '("--fn 'Iosevka 13'"
                                        "--nb '#000000'"
                                        "--nf '#FFFFFF'"
                                        "--tb '#000000'"
                                        "--tf '#FFFFFF'"
                                        "--fb '#000000'"
                                        "--ff '#FFFFFF'"
                                        "--hf '#F0F0F0'"
                                        "--hb '#81A1C1'")
                                      " ")))))
   (service
    home-bash-service-type
    (home-bash-configuration
     (bash-profile
      `("source /home/graves/.dotfiles/home/yggdrasil/files/config/shell/profile"
        ,#~(string-append #$gnupg
                          "/bin/gpg-connect-agent"
                          " updatestartuptty /bye > /dev/null")
        ,#~(string-append "[ $(tty) = /dev/tty1 ] && exec " #$sway "/bin/sway")))))
   (service home-zsh-service-type
            (home-zsh-configuration
             (zprofile
             '("source /home/graves/.dotfiles/home/yggdrasil/files/config/shell/profile" 
	        ,#~(string-append #$gnupg
                          "/bin/gpg-connect-agent"
                          " updatestartuptty /bye > /dev/null")))
	     (zshrc
	      '("source /home/graves/.dotfiles/home/yggdrasil/files/config/zsh/zshrc"))))
   (service home-zsh-direnv-service-type)
   (service home-zsh-autosuggestions-service-type)))
