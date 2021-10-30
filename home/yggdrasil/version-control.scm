(define-module (home yggdrasil version-control)
  #:use-module (guix gexp)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu home services)
  #:use-module (gnu home-services version-control))

(define-public services
  (list
   (service
    home-git-service-type
    (home-git-configuration
     (config
      `((user
         ((name . "Nikita Domnitskii")
          (email . "nikita@domnitskii.me")
          (signingkey . "99465567F17FF3EFD36300348469C699F6646AC6")))
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
        (diff "gpg"
              ((textconv . "gpg --no-tty --decrypt")))
        (sendemail
         ((smtpserver . "smtp.migadu.com")
          (smtpuser . ,(getenv "MIGADU_USER"))
          (smtpencryption . "ssl")
          (smtpserverport . "465")
          (annotate . #t)))))
     (attributes
      '((*.gpg . "filter=gpg diff=gpg")))))))
