(define-module (home yggdrasil mail)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home-services mail)
  #:use-module (home services msmtp)
  #:use-module (home services mu4e))

(define-public services
  (let ((data_home (getenv "XDG_DATA_HOME"))
        (user_nngraves (getenv "USER_NNGRAVES"))
        (user_neleves (getenv "USER_NELEVES"))
        (user_ngmx (getenv "USER_NGMX"))
        (user_ngmail (getenv "USER_NGMAIL"))
        (user_cpure (getenv "USER_CPURE"))
        (user_qpure (getenv "USER_QPURE"))
        (user_pgmail (getenv "USER_PGMAIL")))
    (list
     (service
      home-isync-service-type
      (home-isync-configuration
       (config
        `((IMAPStore ,(string-append user_nngraves "-remote"))
          (Host SSL0.OVH.NET)
          (Port 993)
          (User ,user_nngraves)
          (PassCmd ,(string-append "rbw get " user_nngraves))
          (AuthMechs LOGIN)
          (SSLType IMAPS)
          (CertificateFile /etc/ssl/certs/ca-certificates.crt)
          ,#~""
          (MaildirStore ,(string-append user_nngraves "-local"))
          (Subfolders Legacy)
          (Path ,(string-append data_home "/mail/" user_nngraves "/"))
          (Inbox ,(string-append data_home "/mail/" user_nngraves "/INBOX"))
          ,#~""
          (Channel ,user_nngraves)
          (Expunge Both)
          (Far ,(string-append ":" user_nngraves "-remote:"))
          (Near ,(string-append ":" user_nngraves "-local:"))
          (Patterns * !"Local_Archives")
          (Create Both)
          (SyncState *)
          (MaxMessages 0)
          (ExpireUnread no)
          ,#~""
          ,#~""
          (IMAPStore ,(string-append user_neleves "-remote"))
          (Host messagerie.enpc.fr)
          (Port 993)
          (User ,user_neleves)
          (PassCmd ,(string-append "rbw get " user_neleves))
          (CipherString DEFAULT@SECLEVEL=1)
          (PipelineDepth 1)
          (AuthMechs LOGIN)
          (SSLType IMAPS)
          (CertificateFile /etc/ssl/certs/ca-certificates.crt)
          ,#~""
          (MaildirStore ,(string-append user_neleves "-local"))
          (Subfolders Verbatim)
          (Path ,(string-append data_home "/mail/" user_neleves "/"))
          (Inbox ,(string-append data_home "/mail/" user_neleves "/INBOX"))
          ,#~""
          (Channel ,user_neleves)
          (Expunge Both)
          (Far ,(string-append ":" user_neleves "-remote:"))
          (Near ,(string-append ":" user_neleves "-local:"))
          (Patterns * !"Local_Archives")
          (Create Both)
          (SyncState *)
          (MaxMessages 0)
          (ExpireUnread no)
          ,#~""
          (IMAPStore ,(string-append user_ngmx "-remote"))
          (Host imap.gmx.net)
          (Port 993)
          (User ,user_ngmx)
          (PassCmd ,(string-append "rbw get " user_ngmx))
          (AuthMechs LOGIN)
          (SSLType IMAPS)
          (CertificateFile /etc/ssl/certs/ca-certificates.crt)
          ,#~""
          (MaildirStore ,(string-append user_ngmx "-local"))
          (Subfolders Verbatim)
          (Path ,(string-append data_home "/mail/" user_ngmx "/"))
          (Inbox ,(string-append data_home "/mail/" user_ngmx "/INBOX"))
          ,#~""
          (Channel ,user_ngmx)
          (Expunge Both)
          (Far ,(string-append ":" user_ngmx "-remote:"))
          (Near ,(string-append ":" user_ngmx "-local:"))
          (Patterns * !"Local_Archives")
          (Create Both)
          (SyncState *)
          (MaxMessages 0)
          (ExpireUnread no)
          ,#~""
          ,#~""
          (IMAPStore ,(string-append user_ngmail "-remote"))
          (Host imap.gmail.com)
          (Port 993)
          (User ,user_ngmail)
          (PassCmd ,(string-append "rbw get " user_ngmail))
          (AuthMechs LOGIN)
          (SSLType IMAPS)
          (CertificateFile /etc/ssl/certs/ca-certificates.crt)
          ,#~""
          (MaildirStore ,(string-append user_ngmail "-local"))
          (Subfolders Verbatim)
          (Path ,(string-append data_home "/mail/" user_ngmail "/"))
          (Inbox ,(string-append data_home "/mail/" user_ngmail "/INBOX"))
          ,#~""
          (Channel ,user_ngmail)
          (Expunge Both)
          (Far ,(string-append ":" user_ngmail "-remote:"))
          (Near ,(string-append ":" user_ngmail "-local:"))
          (Patterns * !"[Gmail]/All Mail" !"[Gmail]/Important"
                    !"[Gmail]/Starred" !"[Gmail]/Bin" !"Local_archives")
          (Create Both)
          (SyncState *)
          (MaxMessages 0)
          (ExpireUnread no)
          ,#~""
          (IMAPStore ,(string-append user_cpure "-remote"))
          (Host ssl0.ovh.net)
          (Port 993)
          (User ,user_cpure)
          (PassCmd ,(string-append "rbw get " user_cpure))
          (AuthMechs LOGIN)
          (SSLType IMAPS)
          (CertificateFile /etc/ssl/certs/ca-certificates.crt)
          ,#~""
          (MaildirStore ,(string-append user_cpure "-local"))
          (Subfolders Legacy)
          (Path ,(string-append data_home "/mail/" user_cpure "/"))
          (Inbox ,(string-append data_home "/mail/" user_cpure "/INBOX"))
          ,#~""
          (Channel ,user_cpure)
          (Expunge Both)
          (Far ,(string-append ":" user_cpure "-remote:"))
          (Near ,(string-append ":" user_cpure "-local:"))
          (Patterns *)
          (Create Both)
          (SyncState *)
          (MaxMessages 0)
          (ExpireUnread no)
          ,#~""
          (IMAPStore ,(string-append user_qpure "-remote"))
          (Host pro1.mail.ovh.net)
          (Port 993)
          (User user_qpure)
          (PassCmd ,(string-append "rbw get " user_qpure))
          (AuthMechs LOGIN)
          (SSLType IMAPS)
          (CertificateFile /etc/ssl/certs/ca-certificates.crt)
          ,#~""
          (MaildirStore ,(string-append user_qpure "-local"))
          (Subfolders Verbatim)
          (Path ,(string-append data_home "/mail/" user_qpure "/"))
          (Inbox ,(string-append data_home "/mail/" user_qpure "/INBOX"))
          ,#~""
          (Channel ,user_qpure)
          (Expunge Both)
          (Far ,(string-append ":" user_qpure "-remote:"))
          (Near ,(string-append ":" user_qpure "-local:"))
          (Patterns *)
          (Create Both)
          (SyncState *)
          (MaxMessages 0)
          (ExpireUnread no)
          ,#~""
          ,#~""
          (IMAPStore ,(string-append user_pgmail "-remote"))
          (Host imap.gmail.com)
          (Port 993)
          (User ,user_pgmail)
          (PassCmd ,(string-append "rbw get " user_pgmail))
          (AuthMechs LOGIN)
          (SSLType IMAPS)
          (CertificateFile /etc/ssl/certs/ca-certificates.crt)
          ,#~""
          (MaildirStore ,(string-append user_pgmail "-local"))
          (Subfolders Verbatim)
          (Path ,(string-append data_home "/mail/" user_pgmail "/"))
          (Inbox ,(string-append data_home "/mail/" user_pgmail "/INBOX"))
          ,#~""
          (Channel ,user_pgmail)
          (Expunge Both)
          (Far ,(string-append ":" user_pgmail "-remote:"))
          (Near ,(string-append ":" user_pgmail "-local:"))
          (Patterns * !"[Gmail]/All Mail")
          (Create Both)
          (SyncState *)
          (MaxMessages 0)
          (ExpireUnread no)))))

     (service
      home-msmtp-service-type
      (home-msmtp-configuration
       (config
        `((defaults)
	  (auth on)
          (tls on)
          (tls_trust_file /etc/ssl/certs/ca-certificates.crt)
          (logfile ,(string-append (getenv "XDG_STATE_HOME")
                                   "/msmtp/msmtp.log"))
          ,#~""
          (account ,user_neleves)
          (host boyer2.enpc.fr)
          (port 465)
          (from ,user_neleves)
          (user ,user_neleves)
          (passwordeval ,(string-append "rbw get " user_neleves))
          (tls_starttls off)
          ,#~""
          (account ,user_ngmx)
          (host mail.gmx.net)
          (port 587)
          (from ,user_ngmx)
          (user ,user_ngmx)
          (passwordeval ,(string-append "rbw get " user_ngmx))
          ,#~""
          (account ,user_ngmail)
          (host smtp.gmail.com)
          (port 587)
          (from ,user_ngmail)
          (user ,user_ngmail)
          (passwordeval ,(string-append "rbw get " user_ngmail))
          ,#~""
          (account ,user_cpure)
          (host ssl0.ovh.net)
          (port 465)
          (from ,user_cpure)
          (user ,user_cpure)
          (passwordeval ,(string-append "rbw get " user_cpure))
          (tls_starttls off)
          ,#~""
          (account ,user_nngraves)
          (host ssl0.ovh.net)
          (port 465)
          (from ,user_nngraves)
          (user ,user_nngraves)
          (passwordeval ,(string-append "rbw get " user_nngraves))
          (tls_starttls off)
          ,#~""
          (account ,user_pgmail)
          (host smtp.gmail.com)
          (port 587)
          (from ,user_pgmail)
          (user ,user_pgmail)
          (passwordeval ,(string-append "rbw get " user_pgmail))))))

     (service
      home-mu4e-service-type
      (home-mu4e-configuration
       (config
        `((use-package mu4e-alert
           :defer 20                    ; Wait until 20 seconds after startup
           :config

           ;; Load org-mode integration
           (require 'org-mu4e)

           ;; Refresh mail using isync every 10 minutes
           (setq mu4e-update-interval (* 5 60))
           (setq mu4e-get-mail-command "mbsync -a")
           (setq mu4e-maildir
                 ,(string-append "~/.local/share/mail/" user_nngraves))

           ;; Use Ivy for mu4e completions (maildir folders, etc)
           ;; (setq mu4e-completing-read-function #'ivy-completing-read)
                                        ;FIXME

           ;; Make sure that moving a message (like to Trash) causes the
           ;; message to get a new file name.  This helps to avoid the
           ;; dreaded "UID is N beyond highest assigned" error.
           ;; See this link for more info: https://stackoverflow.com/a/43461973
           (setq mu4e-change-filenames-when-moving t)

           ;; Set up contexts for email accounts
           (setq mu4e-contexts
                 `(,(make-mu4e-context
                     :name ,user_nngraves
                     :match-func (lambda (msg)
                                   (when msg
                                     (string-prefix-p ,(string-append "/" user_nngraves)
                                                      (mu4e-message-field msg :maildir))))
                     :vars '((user-full-name . "Nicolas Graves")
                             (user-mail-address . ,user_nngraves)
                             (mu4e-sent-folder . ,(string-append "/" user_nngraves "/Sent"))
                             (mu4e-trash-folder . ,(string-append "/" user_nngraves "/Trash"))
                             (mu4e-drafts-folder . ,(string-append "/" user_nngraves "/Drafts"))
                             (mu4e-refile-folder . ,(string-append "/" user_nngraves "/Local_Archives"))
                             (mu4e-sent-messages-behavior . sent)))
                   ,(make-mu4e-context
                     :name ,user_neleves
                     :match-func (lambda (msg)
                                   (when msg
                                     (string-prefix-p ,(string-append "/" user_neleves)
                                                      (mu4e-message-field msg :maildir))))
                     :vars '((user-mail-address . ,user_neleves)
                             (mu4e-sent-folder . ,(string-append "/" user_neleves "/Sent"))
                             (mu4e-trash-folder . ,(string-append "/" user_neleves "/Trash"))
                             (mu4e-drafts-folder . ,(string-append "/" user_neleves "/Drafts"))
                             (mu4e-refile-folder . ,(string-append "/" user_neleves "/Local_Archives"))
                             (mu4e-sent-messages-behavior . sent)))
                   ,(make-mu4e-context
                     :name ,user_ngmx
                     :match-func (lambda (msg)
                                     (when msg
                                       (string-prefix-p ,(string-append "/" user_ngmx)
                                                        (mu4e-message-field msg :maildir))))
                     :vars '((user-mail-address . ,user_ngmx)
                             (mu4e-sent-folder . ,(string-append "/" user_ngmx "/Envoy&AOk-s"))
                             (mu4e-trash-folder . ,(string-append "/" user_ngmx "/Corbeille"))
                             (mu4e-drafts-folder . ,(string-append "/" user_ngmx "/Brouillons"))
                             (mu4e-refile-folder . ,(string-append "/" user_ngmx "/Local_Archives"))
                             (mu4e-sent-messages-behavior . sent)))
                   ,(make-mu4e-context
                     :name ,user_ngmail
                     :match-func (lambda (msg)
                                   (when msg
                                     (string-prefix-p ,(string-append "/" user_ngmail)
                                                      (mu4e-message-field msg :maildir))))
                     :vars '((user-mail-address . ,user_ngmail)
                             (mu4e-sent-folder . ,(string-append "/" user_ngmail "/[Gmail]/Sent Mail"))
                             (mu4e-trash-folder . ,(string-append "/" user_ngmail "/[Gmail]/Trash"))
                             (mu4e-drafts-folder . ,(string-append "/" user_ngmail "/[Gmail]/Drafts"))
                             (mu4e-refile-folder . ,(string-append "/" user_ngmail "/[Gmail]/Local_Archives"))
                             (mu4e-sent-messages-behavior . sent)))
                   ))
           (setq mu4e-context-policy 'pick-first)

           ;; Prevent mu4e from permanently deleting trashed items
           ;; This snippet was taken from the following article:
           ;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
           ;; (defun remove-nth-element (nth list)
           ;;   (if (zerop nth) (cdr list)
           ;;       (let ((last (nthcdr (1- nth) list))) ;FIXME
           ;;         (setcdr last (cddr last))
           ;;         list)))
           ;; (setq mu4e-marks (remove-nth-element 5 mu4e-marks))
           ;; (add-to-list 'mu4e-marks
           ;;              '(trash
           ;;                :char ("d" . "▼")
           ;;                :prompt "dtrash"
           ;;                :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
           ;;                :action (lambda (docid msg target)
           ;;                          (mu4e~proc-move docid
           ;;                                          (mu4e~mark-check-target target) "-N"))))

           ;; Display options
           (setq mu4e-view-show-images t)
           (setq mu4e-view-show-addresses 't)

           ;; Composing mail
           (setq mu4e-compose-dont-reply-to-self t)

           ;; Use mu4e for sending e-mail
           (setq sendmail-program "/home/graves/.guix-profile/bin/msmtp"
                 message-send-mail-function 'smtpmail-send-it
                 message-sendmail-f-is-evil t
                 message-sendmail-extra-arguments '("--read-envelope-from")
                 message-send-mail-function 'message-send-mail-with-sendmail)

           ;; Signing messages (use mml-secure-sign-pgpmime)
           (setq mml-secure-openpgp-signers '("7B4A11D39E3BB804BA28F1B05E21AA8964E23B75"))

           ;; (See the documentation for `mu4e-sent-messages-behavior' if you have
           ;; additional non-Gmail addresses and want assign them different
           ;; behavior.)

           ;; setup some handy shortcuts
           ;; you can quickly switch to your Inbox -- press ``ji''
           ;; then, when you want archive some messages, move them to
           ;; the 'All Mail' folder by pressing ``ma''.
           (setq mu4e-maildir-shortcuts
                 '((,(string-append "/" user_nngraves "/INBOX")       . ?i)
                   (,(string-append "/" user_nngraves "/Lists/*")     . ?l)
                   (,(string-append "/" user_nngraves "/Sent Mail")   . ?s)
                   (,(string-append "/" user_nngraves "/Trash")       . ?t)))

           (add-to-list 'mu4e-bookmarks
                        (make-mu4e-bookmark
                         :name "All Inboxes"
                         :query
                         ,(string-append "maildir:/" user_nngraves "/INBOX" " OR "
                                         "maildir:/" user_neleves "/Inbox")
                         :key ?i))

           ;; don't keep message buffers around
           (setq message-kill-buffer-on-exit t)

           (setq ng/mu4e-inbox-query
                 ,(string-append "(maildir:/" user_nngraves "/INBOX" " OR "
                                 "maildir:/" user_neleves "/Inbox)" " AND "
                                 "flag:unread"))

           (defun ng/go-to-inbox ()
             (interactive)
             (mu4e-headers-search ng/mu4e-inbox-query))

           (ng/leader-key-def
            "m"  '(:ignore t :which-key "mail")
            "mm" 'mu4e
            "mc" 'mu4e-compose-new
            "mi" 'ng/go-to-inbox
            "ms" 'mu4e-update-mail-and-index)

           ;; Start mu4e in the background so that it syncs mail periodically
           (mu4e t)

           (setq mu4e-alert-interesting-mail-query ng/mu4e-inbox-query)

           ;; Show notifications for mails already notified
           (mu4e-alert-enable-notifications)
           ;; (setq mu4e-alert-notify-repeated-mails nil)
           )))))
     )))

;; Mail in Emacs with mu4e

;; [[http://www.djcbsoftware.nl/code/mu/mu4e.html][mu4e]] is the best mail interface I've ever used because it's fast and makes it really easy to power through a huge e-mail backlog.  Love the ability to capture links to emails with org-mode too.

;; Useful mu4e manual pages:

;; - [[https://www.djcbsoftware.nl/code/mu/mu4e/MSGV-Keybindings.html#MSGV-Keybindings][Key bindings]]
;; - [[https://www.djcbsoftware.nl/code/mu/mu4e/Org_002dmode-links.html#Org_002dmode-links][org-mode integration]]

;; And use [[https://github.com/iqbalansari/mu4e-alert][mu4e-alert]] to show notifications when e-mail comes in.
;; There are slight difference with Daviwil's dotfiles, since I get an error for the non-existing emacs-mu4e package ; I just included it in mu4e-alert instead.
