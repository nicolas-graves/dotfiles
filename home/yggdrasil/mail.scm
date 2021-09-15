(define-module (home yggdrasil mail)
  #:use-module (guix gexp)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services mail))

(define-public services
  (list
   (service home-isync-service-type
            (home-isync-configuration
             (config
              `((IMAPAccount private-remote)
                (Host "imap.migadu.com")
                (User ,(getenv "MIGADU_USER_ALT"))
                (Pass ,(getenv "MIGADU_PASS_ALT"))
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
                (Host "imap.migadu.com")
                (User ,(getenv "MIGADU_USER"))
                (Pass ,(getenv "MIGADU_PASS"))
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
                (Host "imap.gmail.com")
                (User ,(getenv "GMAIL_USER"))
                (Pass ,(getenv "GMAIL_PASS"))
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
                 ((path . "mail")
                  (mail_root . "mail")))
                (maildir
                 ((synchronize_flags . true)))
                (new
                 ((tags . new)
                  (ignore . (.mbsyncstate .uidvalidity))))))))
   #;
   (service home-l2md-service-type
            (home-l2md-configuration))
   ))
