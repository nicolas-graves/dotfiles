(define-module (server cuirass)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (guix gexp)

  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system file-systems)

  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services cuirass)
  #:use-module (gnu services avahi)
  #:use-module (gnu services web)
  #:use-module (gnu services certbot)

  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages ci)
  #:use-module (gnu packages avahi))

;; rde has been defined as a dependency of the graves channel
;; should be build automatically

(define-public packages
  '("cuirass" "nss-certs" "certbot" "nginx"))

(define %cuirass-specs
  #~(list
     (specification
      (name "mychannelsv10")
      (build '(packages "emacs-pgtk-native-comp"
                        "linux"
                        "rbw@1.4.3"
                        "linux-firmware"
                        "alacritty"))
      (channels
       (cons*
        (channel
         (name 'graves)
         (url "https://github.com/nicolas-graves/guix-channel.git"))
        (channel
         (name 'rde)
         (url "https://git.sr.ht/~abcdw/rde"))
        (channel
         (name 'flat)
         (url "https://github.com/flatwhatson/guix-channel.git"))
        (channel
         (name 'nonguix)
         (url "https://gitlab.com/nonguix/nonguix"))
        %default-channels)))))

(define %nginx-deploy-hook
  (program-file
   "nginx-deploy-hook"
   #~(let ((pid (call-with-input-file "/var/run/nginx/pid" read)))
       (kill pid SIGHUP))))


(define-public services
  (let ((cuirass_uri (getenv "URI_service_cuirass"))
        (substitutes_uri (getenv "URI_service_substitutes")))
    (list
     (service avahi-service-type)
     (service cuirass-service-type
              (cuirass-configuration
               (interval 60)
               (remote-server #f)
               (database "dbname=cuirass host=/var/run/postgresql")
               (port (string->number (getenv "PORT_service_cuirass")))
               (host "localhost")
               (specifications %cuirass-specs)
               (use-substitutes? #t)
               (one-shot? #f)
               (fallback? #t)
               (extra-options '())))
     (extra-special-file
      (string-append "/" (getenv "ID_ssh_my_server")
                     "/.dotfiles/keys/" (getenv "KEY_ssh_my_server"))
      (local-file
       (string-append (getenv "HOME") "/.dotfiles/keys/"
                      (getenv "KEY_ssh_my_server"))))
     (service nginx-service-type
              (nginx-configuration
               (server-blocks
                (list (nginx-server-configuration
                       (listen (list "443 ssl"))
                       (server-name (list cuirass_uri))
                       (ssl-certificate
                        (string-append "/etc/letsencrypt/live/" cuirass_uri "/fullchain.pem"))
                       (ssl-certificate-key
                        (string-append "/etc/letsencrypt/live/" cuirass_uri "/privkey.pem"))
                       (locations
                        (list
                         (nginx-location-configuration
                          (uri "/")
                          (body (list (string-append
                                    "proxy_pass http://127.0.0.1:"
                                    (getenv "PORT_service_cuirass") ";")))))))
                      (nginx-server-configuration
                       (listen (list "443 ssl"))
                       (server-name (list substitutes_uri))
                       (ssl-certificate
                        (string-append "/etc/letsencrypt/live/" substitutes_uri "/fullchain.pem"))
                       (ssl-certificate-key
                        (string-append "/etc/letsencrypt/live/" substitutes_uri "/privkey.pem"))
                       (locations
                        (list
                         (nginx-location-configuration
                          (uri "/")
                          (body (list (string-append
                                    "proxy_pass http://127.0.0.1:"
                                    (getenv "PORT_service_substitutes") ";")))))))
                      ))))
     (service guix-publish-service-type
              (guix-publish-configuration
               (port (string->number (getenv "PORT_service_substitutes")))
               (ttl 300)))
     (service certbot-service-type
              (certbot-configuration
               (email (getenv "USER_NNGRAVES"))
               (certificates
                (list
                 (certificate-configuration
                  (domains (list cuirass_uri substitutes_uri))
                  (deploy-hook %nginx-deploy-hook)))))))))
