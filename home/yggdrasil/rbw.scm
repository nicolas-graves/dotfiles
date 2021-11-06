(define-module (home yggdrasil rbw)
  #:use-module (json)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (gnu home-services-utils))

(define-json-type <account>
  (email)
  (base_url)
  (identity_url)
  (lock_timeout)
  (pinentry))

(define-public rbw-config
  (let ((uri_bitwarden (getenv "URI_service_bitwarden"))
        (uri_vaultwarden (getenv "URI_service_vaultwarden")))
    (list
     `(,(string-append "config/rbw/" uri_bitwarden ".config.json")
       ,(plain-file
         uri_bitwarden
         (account->json
          (make-account (getenv "USER_service_bitwarden")
                        (string-append "https://" uri_bitwarden)
                        'null
                        86400
                        "pinentry-qt"))))
     `(,(string-append "config/rbw/" uri_vaultwarden ".config.json")
       ,(plain-file
         uri_vaultwarden
         (account->json
          (make-account (getenv "USER_service_bitwarden")
                        (string-append "https://" uri_vaultwarden)
                        'null
                        3600
                        "pinentry-qt")))))))
