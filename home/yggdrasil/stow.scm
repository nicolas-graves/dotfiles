(define-module (home yggdrasil stow)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (home yggdrasil rbw)
  #:use-module (gnu home-services-utils))

(define-public services
  (list
   (service
    home-files-service-type
    (append
     (list
      `("local/bin" ,(local-file "files/scripts" #:recursive? #t))
      `("local/share" ,(local-file "files/share" #:recursive? #t))
      `("config/zathura/zathurarc" ,(local-file "files/config/zathura/zathurarc"))
      `("config/wget/wgetrc" ,(local-file "files/config/wget/wgetrc"))
      `("config/youtube-viewer" ,(local-file "files/config/youtube-viewer" #:recursive? #t))
      `("config/mpv" ,(local-file "files/config/mpv" #:recursive? #t)))
     rbw-config))))
