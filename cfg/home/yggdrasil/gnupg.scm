(define-module (home yggdrasil gnupg)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services gnupg)
  #:use-module (gnu home-services password-utils))

(define-public services
  (list
   (service
    home-gnupg-service-type
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
          (with-subkey-fingerprint . #t)
          (with-keygrip . #t)))))
     (gpg-agent-config
      (home-gpg-agent-configuration
       (ssh-agent? #t)
       (pinentry-flavor 'bemenu)
       (ssh-keys '(("B0922A971719E1CB253E38DC4357F5C6084DBA3C")))
       (extra-config
        '((display . ":0")))))))))
