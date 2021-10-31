(define-module (home yggdrasil mail)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home-services mail)
  #:use-module (home services msmtp))

(define-public services
  (list
   (service home-isync-service-type
            (home-isync-configuration
             (config
              `((IMAPAccount private-remote)
                (Host imap.migadu.com)
                (User ,(getenv "MIGADU_USER_ALT"))
                (PassCmd "pass show mail/private")
                (SSLType IMAPS)
                ,#~""
                (MaildirStore private-local)
                (Path "~/docs/mail/private/")
                (INBOX "~/docs/mail/private/INBOX")
                (SubFolders Verbatim)
                ,#~""
                (IMAPStore private-remote)
                (Account private-remote)
                ,#~""
                (Channel private)
                (Far ":private-remote:")
                (Near ":private-local:")
                (Patterns *)
                (Create Both)
                (Expunge Both)
                ,#~""
                (IMAPAccount public-remote)
                (Host imap.migadu.com)
                (User ,(getenv "MIGADU_USER"))
                (PassCmd "pass show mail/public")
                (SSLType IMAPS)
                ,#~""
                (MaildirStore public-local)
                (Path "~/docs/mail/public/")
                (INBOX "~/docs/mail/public/INBOX")
                (SubFolders Verbatim)
                ,#~""
                (IMAPStore public-remote)
                (Account public-remote)
                ,#~""
                (Channel public)
                (Far ":public-remote:")
                (Near ":public-local:")
                (Patterns *)
                (Create Both)
                (Expunge Both)
                ,#~""
                (IMAPAccount work-remote)
                (Host imap.gmail.com)
                (User ,(getenv "GMAIL_USER"))
                (Pass "pass show work/health-samurai")
                (SSLType IMAPS)
                ,#~""
                (MaildirStore work-local)
                (Path "~/docs/mail/work/")
                (INBOX "~/docs/mail/work/INBOX")
                (SubFolders Verbatim)
                ,#~""
                (IMAPStore work-remote)
                (Account work-remote)
                ,#~""
                (Channel work-default)
                (Far ":work-remote:")
                (Near ":work-local:")
                (Patterns * "![Gmail]*")
                (Create Both)
                (Expunge Both)
                (SyncState *)
                ,#~""
                (Channel work-sent)
                (Far ":work-remote:[Gmail]/Sent Mail")
                (Near ":work-local:Sent")
                (Create Both)
                (Expunge Both)
                (SyncState *)
                ,#~""
                (Group work)
                (Channel work-default)
                (Channel work-sent)))))

   (service home-notmuch-service-type
            (home-notmuch-configuration
             (config
              `((user
                 ((name . "Nikita Domnitskii")
                  (primary_email . ,(getenv "MIGADU_USER"))))
                (database
                 ((mail_root . "docs/mail/")
                  (path . "docs/mail/")))
                (maildir
                 ((synchronize_flags . true)))
                (new
                 ((tags . new)
                  (ignore . (.mbsyncstate .uidvalidity))))))))

   (service home-msmtp-service-type
            (home-msmtp-configuration
             (config
              `((defaults)
                (auth on)
                (tls on)
                (tls_starttls off)
                (logfile ,(string-append (getenv "XDG_LOG_HOME") "/msmtp.log"))
                ,#~""
                (account public)
                (host smtp.migadu.com)
                (port 465)
                (from ,(getenv "MIGADU_USER"))
                (user ,(getenv "MIGADU_USER"))
                (passwordeval "pass show mail/public")
                ,#~""
                (account default : public)))))
   ;; (service home-l2md-service-type
   ;;          (home-l2md-configuration))
   ))
