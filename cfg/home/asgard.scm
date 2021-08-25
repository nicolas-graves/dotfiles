(define-module (home asgard)
  #:use-module (guix gexp)
  #:use-module (guix transformations)

  #:use-module (gnu home)

  #:use-module (gnu services)

  #:use-module (gnu packages)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages emacs)

  #:use-module (kreved home-services dbus)
  #:use-module (kreved home-services pipewire)
  #:use-module (kreved home-services shellutils)

  #:use-module (rde packages)

  #:use-module (gnu home-services)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu home-services gnupg)
  #:use-module (gnu home-services version-control)
  #:use-module (gnu home-services ssh)
  #:use-module (gnu home-services xdg)
  #:use-module (gnu home-services shells)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home-services wm)
  #:use-module (gnu home-services mail)
  #:use-module (gnu home-services password-utils))

(define transform
  (options->transformation
   '((with-commit . "emacs-evil=ad47644eea5e351269f5bead18e713768d96f207")
     (with-commit . "emacs-icomplete-vertical=3bee30b374226deecde8a5cbbc6ca8471c303348")
     (with-commit . "emacs-use-package=a7422fb8ab1baee19adb2717b5b47b9c3812a84c")
     (with-commit . "emacs-embark=acbe1cba548832d295449da348719f69b9685c6f"))))

(home-environment
 (packages
  (map (compose list specification->package+output)
       '("ungoogled-chromium-wayland" "firefox-wayland" "telegram-desktop"
         "flatpak" "pavucontrol" "bluez" "alacritty" "xdg-utils"
         "font-iosevka" "font-openmoji" "font-google-roboto"
         "font-google-noto" "wofi" "bemenu" "mako" "i3status"
         "swappy" "grim" "slurp" "wl-clipboard" "hicolor-icon-theme"
         "adwaita-icon-theme" "gnome-themes-standard" "git:send-email"
         "xdg-desktop-portal" "xdg-desktop-portal-wlr" "direnv"
         "swayidle" "nyxt" "cl-slynk" "gstreamer" "gst-libav"
         "gst-plugins-base" "gst-plugins-good" "gst-plugins-bad"
         "gst-plugins-ugly")))
 (services
  (list
   (service home-ssh-service-type)
   (service home-bash-service-type
            (home-bash-configuration
             (bash-profile
              '("source /run/current-system/profile/etc/profile.d/nix.sh"))
             (environment-variables
              '(("XDG_CURRENT_DESKTOP" . "sway")
                ("XDG_SESSION_TYPE" . "wayland")
                ("QT_QPA_PLATFORM" . "wayland-egl")))))
   (service home-bash-direnv-service-type)
   (service home-dbus-service-type)
   (service home-pipewire-service-type)
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
                  (default-preference-list . ("SHA512" "SHA384" "SHA256"
                                              "SHA224" "AES256" "AES192"
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
                '((display . ":0")))))))

   (service home-git-service-type
            (home-git-configuration
             (config
              `((user
                 ((name . "Nikita Domnitskii")
                  (email . "nikita@domnitskii.me")
                  (signingkey . "33EBAD5593EE044A109259E7866D47A1F8153C7F")))
                (gpg
                 ((program . ,(file-append gnupg "/bin/gpg"))))
                (commit
                 ((gpgsign . #t)))
                (tag
                 ((gpgsign . #t)))
                (pull
                 ((rebase . #t)))
                (github
                 ((user . "krevedkokun")))
                (sendemail
                 ((smtpserver . "smtp.migadu.com")
                  (smtpuser . ,(getenv "MIGADU_USER"))
                  (smtpencryption . "ssl")
                  (smtpserverport . "465")
                  (annotate . #t)))))
             (ignore
              '("**/.envrc" "**/.direnv"))))

   (service home-emacs-service-type
            (home-emacs-configuration
             (package emacs-next-pgtk-latest)
             (rebuild-elisp-packages? #t)
             (init-el
              `(,(slurp-file-gexp (local-file "files/init.el"))))
             (elisp-packages
              (append
               (map (compose transform specification->package)
                    '("emacs-orderless" "emacs-modus-themes" "emacs-which-key"
                      "emacs-eros" "emacs-gcmh" "emacs-minions" "emacs-async"
                      "emacs-marginalia" "emacs-rg" "emacs-nov-el"
                      "emacs-pdf-tools" "emacs-eglot" "emacs-docker"
                      "emacs-dockerfile-mode" "emacs-docker-compose-mode"
                      "emacs-restclient" "emacs-macrostep" "emacs-csv-mode"
                      "emacs-consult" "emacs-project" "emacs-erc-image"
                      "emacs-erc-hl-nicks" "emacs-clojure-mode" "emacs-cider"
                      "emacs-helpful" "emacs-geiser" "emacs-geiser-guile"
                      "emacs-magit" "emacs-geiser-eros" "emacs-flymake-kondor"
                      "emacs-use-package" "emacs-direnv" "emacs-mini-frame"
                      "emacs-embark" "emacs-sly" "emacs-paredit"
                      #;"emacs-git-email"))))))

   (service home-sway-service-type
            (home-sway-configuration
             (config
              `((include ,(local-file "files/sway"))))))

   (service home-isync-service-type
            (home-isync-configuration
             (config
              `((IMAPAccount private-remote)
                (Host "imap.migadu.com")
                (User ,(getenv "MIGADU_USER"))
                (Pass ,(getenv "MIGADU_PASS"))
                (SSLType IMAPS)
                ,#~""
                (MaildirStore private-local)
                (Path "~/mail/private/")
                (INBOX "~/mail/private/INBOX")
                (SubFolders Verbatim)
                ,#~""
                (IMAPStore private-remote)
                (Account private-remote)
                ,#~""
                (Channel private)
                (Far ":private-remote:")
                (Near ":private-local:")
                (Patterns *)
                (Create Both)
                (Expunge Both)
                ,#~""
                (IMAPAccount public-remote)
                (Host "imap.migadu.com")
                (User ,(getenv "MIGADU_USER2"))
                (Pass ,(getenv "MIGADU_PASS2"))
                (SSLType IMAPS)
                ,#~""
                (MaildirStore public-local)
                (Path "~/mail/public/")
                (INBOX "~/mail/public/INBOX")
                (SubFolders Verbatim)
                ,#~""
                (IMAPStore public-remote)
                (Account public-remote)
                ,#~""
                (Channel public)
                (Far ":public-remote:")
                (Near ":public-local:")
                (Patterns *)
                (Create Both)
                (Expunge Both)
                ,#~""
                (IMAPAccount work-remote)
                (Host "imap.gmail.com")
                (User ,(getenv "GMAIL_USER"))
                (Pass ,(getenv "GMAIL_PASS"))
                (SSLType IMAPS)
                ,#~""
                (MaildirStore work-local)
                (Path "~/mail/work/")
                (INBOX "~/mail/work/INBOX")
                (SubFolders Verbatim)
                ,#~""
                (IMAPStore work-remote)
                (Account work-remote)
                ,#~""
                (Channel work-default)
                (Far ":work-remote:")
                (Near ":work-local:")
                (Patterns * "![Gmail]*")
                (Create Both)
                (Expunge Both)
                (SyncState *)
                ,#~""
                (Channel work-sent)
                (Far ":work-remote:[Gmail]/Sent Mail")
                (Near ":work-local:Sent")
                (Create Both)
                (Expunge Both)
                (SyncState *)
                ,#~""
                (Group work)
                (Channel work-default)
                (Channel work-sent)))))

   (service home-notmuch-service-type
            (home-notmuch-configuration
             (config
              `((user
                 ((name . "Nikita Domnitskii")
                  (primary_email . ,(getenv "MIGADU_USER"))))
                (database
                 ((path . "mail")
                  (mail_root . "mail")))
                (maildir
                 ((synchronize_flags . true)))
                (new
                 ((tags . new)
                  (ignore . (.mbsyncstate .uidvalidity))))))))

   #;(service home-l2md-service-type
   (home-l2md-configuration))

   (service home-password-store-service-type
            (home-password-store-configuration
             (browserpass-native? #t))))))
