(define-module (home asgard)
  #:use-module (guix gexp)
  #:use-module (guix transformations)

  #:use-module (gnu home)

  #:use-module (gnu services)

  #:use-module (gnu packages)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages emacs)

  #:use-module (gnu home-services)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu home-services gnupg)
  #:use-module (gnu home-services version-control)
  #:use-module (gnu home-services fontutils)
  #:use-module (gnu home-services ssh)
  #:use-module (gnu home-services xdg)
  #:use-module (gnu home-services shells)
  #:use-module (gnu home-services emacs))

(define transform
  (options->transformation
   '((with-commit . "emacs-evil=ad47644eea5e351269f5bead18e713768d96f207")
     (with-commit . "emacs-icomplete-vertical=3bee30b374226deecde8a5cbbc6ca8471c303348")
     (with-commit . "emacs-consult=f1ae2244da20702525fe2991076322b9c6b34202")
     (with-commit . "emacs-use-package=a7422fb8ab1baee19adb2717b5b47b9c3812a84c"))))

(home-environment
 (packages
  (map specification->package+output
       '("ungoogled-chromium-wayland" "telegram-desktop"
         "flatpak" "pavucontrol" "bluez" "alacritty"
         "font-iosevka" "font-openmoji" "font-awesome"
         "font-google-roboto" "wofi" "bemenu" "mako"
         "i3status" "grim" "slurp" "wl-clipboard" )))
 (services
  (list
   (service home-ssh-service-type)
   (service home-bash-service-type)
   (simple-service 'my-env
                   home-environment-variables-service-type
                   '(("DISPLAY" . ":0")
                     ("XDG_CURRENT_DESKTOP" . "sway")
                     ("XDG_SESSION_TYPE" . "wayland")
                     ("RTC_USE_PIPEWIRE" . "true")
                     ("SDL_VIDEODRIVER" . "wayland")
                     ("MOZ_ENABLE_WAYLAND" . "1")
                     ("CLUTTER_BACKEND" . "wayland")
                     ("ELM_ENGINE" . "wayland_egl")
                     ("ECORE_EVAS_ENGINE" . "wayland-egl")
                     ("QT_QPA_PLATFORM" . "wayland-egl")))
   (service home-xdg-mime-applications-service-type
            (home-xdg-mime-applications-configuration
             (default '((x-scheme-handler/http . chromium.desktop)
                        (x-scheme-handler/https . chromium.desktop)))))
   (service home-xdg-user-directories-service-type
            (home-xdg-user-directories-configuration
             (download "$HOME/dls")
             (videos "$HOME/video")
             (music "$HOME/music")
             (pictures "$HOME/img")
             (documents "$HOME/docs")
             (publicshare "$HOME")
             (templates "$HOME")
             (desktop "$HOME")))
   (service home-gnupg-service-type
            (home-gnupg-configuration
             (gpg-config
              (home-gpg-configuration
               (extra-config
                '((cert-digest-algo . "SHA256")
                  (default-preference-list . ("SHA512"
                                              "SHA384"
                                              "SHA256"
                                              "SHA224"
                                              "AES256"
                                              "AES192"
                                              "Uncompressed"))
                  (keyserver . "keys.openpgp.org")
                  (keyid-format . long)
                  (with-subkey-fingerprint . #t)))))
             (gpg-agent-config
              (home-gpg-agent-configuration
               (ssh-agent? #t)
               (pinentry-flavor 'qt)
               (ssh-keys '(("F0783042DD8DD697C99A1B9D8D6A82AC8A075F91")))
               (extra-config
                '((default-cache-ttl . 1)
                  (max-cache-ttl . 1)))))))
   (service home-git-service-type
            (home-git-configuration
             (config
              `((user
                 ((name . "Nikita Domnitskii")
                  (email . "nikita@domnitskii.me")
                  (signingkey . "33EBAD5593EE044A109259E7866D47A1F8153C7F")))
                (gpg
                 ((program . ,(file-append gnupg "/bin/gpg"))))
                (tag
                 ((gpgsign . #t)))
                (commit
                 ((gpgsign . #t)))
                (pull
                 ((rebase . #t)))
                (github
                 ((user . "krevedkokun")))
                (sendemail
                 ((smtpserver . "smtp.migadu.com")
                  (smtpuser . "notrelevant")
                  (smtpencryption . "ssl")
                  (smtpserverport . "465")
                  (supresscc . "self")))))))
   (service home-emacs-service-type
            (home-emacs-configuration
             (package emacs-next-pgtk)
             (rebuild-elisp-packages? #t)
             (init-el
              `(,(slurp-file-gexp (local-file "files/init.el"))))
             (elisp-packages
              (append
               (map (compose transform specification->package)
                    '("emacs-orderless" "emacs-modus-themes"
                      "emacs-which-key" "emacs-eros" "emacs-gcmh"
                      "emacs-minions" "emacs-async" "emacs-marginalia"
                      "emacs-rg" "emacs-nov-el" "emacs-pdf-tools"
                      "emacs-eglot" "emacs-docker" "emacs-dockerfile-mode"
                      "emacs-docker-compose-mode" "emacs-restclient"
                      "emacs-macrostep" "emacs-csv-mode" "emacs-consult"
                      "emacs-project" "emacs-erc-image" "emacs-erc-hl-nicks"
                      "emacs-clojure-mode" "emacs-cider" "emacs-helpful"
                      "emacs-geiser" "emacs-geiser-guile" "emacs-moody"
                      "emacs-magit" "emacs-geiser-eros" "emacs-flymake-kondor"
                      "emacs-flimenu" "emacs-use-package" "emacs-evil"
                      "emacs-evil-collection" "emacs-evil-cleverparens"
                      "emacs-evil-commentary" "emacs-evil-surround"
                      "emacs-icomplete-vertical" "emacs-kubel")))))))))
