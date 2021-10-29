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
         ((name . "Nicolas Graves")
          (email . "ngraves@ngraves.fr")
          (signingkey . "7B4A11D39E3BB804BA28F1B05E21AA8964E23B75")))
        (gpg
         ((program . ,(file-append gnupg "/bin/gpg"))))
        (commit
         ((gpgsign . #t)))
        (tag
         ((gpgsign . #t)))
        (pull
         ((rebase . #t)))
        (github
         ((user . "nicolas-graves")))
        (diff "gpg"
              ((textconv . "gpg --no-tty --decrypt")))
        (sendemail
         ((smtpserver . "smtp.migadu.com")
          (smtpuser . ,(getenv "MIGADU_USER"))
          (smtpencryption . "ssl")
          (smtpserverport . "465")
          (annotate . #t)))))))))
