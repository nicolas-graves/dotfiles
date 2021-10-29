(define-module (home yggdrasil gnupg)
  #:use-module (gnu home services)
  #:use-module (gnu home-services gnupg))

(define-public services
  (list
   (service
    home-gnupg-service-type
    (home-gnupg-configuration
     (gpg-config
      (home-gpg-configuration
       (extra-config
        '((cert-digest-algo . SHA512)
          (default-preference-list
            .
            (SHA512 SHA384 SHA256 AES256 AES192 AES ZLIB BZIP2 ZIP Uncompressed))
          (personal-cipher-preferences . (AES256 AES192 AES))
          (personal-digest-preferences . (SHA512 SHA384 SHA256))
          (personal-compress-preferences . (ZLIB BZIP2 ZIP Uncompressed))
          (keyserver . "keys.openpgp.org")
          (keyid-format . long)
          (with-subkey-fingerprint . #t)
          (with-keygrip . #t)))))
     (gpg-agent-config
      (home-gpg-agent-configuration
       (ssh-agent? #t)
       (pinentry-flavor 'bemenu)
       (ssh-keys '(("7B4A11D39E3BB804BA28F1B05E21AA8964E23B75")))))))))
