(define-module (home yggdrasil xdg)
  #:use-module (gnu home-services)
  #:use-module (gnu home services xdg))

(define-public services
  (list
   (service home-xdg-mime-applications-service-type
            (home-xdg-mime-applications-configuration
             (default
               '((x-scheme-handler/http . chromium.desktop)
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
             (desktop "$HOME")))))
