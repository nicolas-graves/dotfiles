(define-module (home services maildirs)
  #:export (%nested-dirs))

(define %nested-dirs
  `((,(getenv "USER_NNGRAVES")
     ("Drafts"
      "INBOX"
      "INBOX.Drafts"
      "INBOX.Junk"
      "INBOX.Local_Archives"
      "INBOX.Sent"
      "INBOX.Templates"
      "INBOX.Trash"
      "Junk"
      "Local_Archives"
      "Sent"
      "Templates"
      "Trash"))
    (,(getenv "USER_NELEVES")
     ("Archives"
      "Chats"
      "Contacts"
      "Drafts"
      "Emailed\\ Contacts"
      "INBOX"
      "Junk"
      "Local_Archives"
      "Sent"
      "Templates"
      "Trash"))
    (,(getenv "USER_NGMX")
     ("Brouillons"
      "Corbeille"
      "Dossier\\ Spam"
      "Envoy\\&AOk-s"
      "Important"
      "INBOX"
      "Local_Archives"
      "OUTBOX"
      "Trash"
      "Unwanted"))
    (,(getenv "USER_GCC")
     ("Archives"
      "Drafts"
      "INBOX"
      "Junk"
      "Sent"
      "Templates"
      "Trash"))
    (,(getenv "USER_NGMAIL")
     ("[Gmail].Brouillons"
      "[Gmail].Corbeille"
      "[Gmail].Drafts"
      "[Gmail].Important"
      "envoy\\&AOk-s"
      "Mail"
      "[Gmail].Spam"
      "[Gmail].Starred"
      "[Gmail].Suivis"
      "messages"
      "[Gmail].Trash"
      "INBOX"
      "Local_Archives"))))
