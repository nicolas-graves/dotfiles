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

(define %cuirass-specs
  #~(list
     (specification
      (name "mychannels")
      (build '(channels graves kreved rde))
      (channels
       (append
        (list
         (channel
          (name 'graves)
          (url "https://github.com/nicolas-graves/guix-channel.git"))
         (channel
          (name 'rde)
          (url "https://git.sr.ht/~abcdw/rde")
          (introduction
           (make-channel-introduction
            "257cebd587b66e4d865b3537a9a88cccd7107c95"
            (openpgp-fingerprint
             "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
         (channel
          (name 'kreved)
          (url "https://git.sr.ht/~krevedkokun/guix-channel")))
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
       (string-append "./keys/" (getenv "KEY_ssh_my_server"))))
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
