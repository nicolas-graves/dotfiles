(define-module (server packages)
  #:use-module (gnu packages))

(define-public packages
  (map (compose list specification->package+output)
       '("git"
         "cuirass"
         "nss-certs"
         "htop"
         "certbot"
         "nginx"
         "rsync"
         )))
