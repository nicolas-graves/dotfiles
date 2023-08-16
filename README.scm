;; -*- mode: scheme -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2022, 2023 Nicolas Graves <ngraves@ngraves.fr>
;; The tooling associated to run this config is in ./make.
;; The commands are ./make channels/pull/home/system/all.
;; They allow additional arguments which are passed to guix commands (e.g. -K).


;; Introduction

;; I embraced the concept of system crafting for long-term efficiency reasons.
;; Despite my satisfaction with Arch, its lack of portability became evident when
;; my computer failed.

;; This repository holds all my configurations, made clean, compact and simple by
;; Andrew Tropin's RDE and GNU Guix. It shouldn't be tedious to maintain your own
;; dotfiles! It's thus a resource for cherry-picking code snippets for RDE users.

;; You can find more about RDE right here : https://git.sr.ht/~abcdw/rde


;;; Channels (https://guix.gnu.org/manual/en/html_node/Channels.html#Channels)

;; To develop Guix/RDE rapidly, I use local repositories and optimizations in ./make.
;; ./make channels generates `~/.config/guix/channels.scm'
;; Pin with the following commit field (TODO repare).
;; This allows to keep some previously working commits so that you can downgrade easily.
;; Tip: to sign commits when broken: `git --no-gpg-sign'

(define %channels
  (list
   (channel
    (inherit %default-guix)
    ;; (commit "c5fa9dd0e96493307cc76ea098a6bca9b076e012")
    (url "~/spheres/info/guix"))
   (channel
    (inherit %default-nonguix)
    ;; (commit "e026dba1dad924aa09da8a28caa343a8ace3f6c7")
    (url "~/spheres/info/nonguix"))
   (channel
    (inherit %default-rde)
    ;; (commit "74a3fb8378e86603bb0f70b260cbf46286693392")
    (url "~/spheres/info/rde"))))


;;; Hardware/Host file systems
;; BTRFS + LUKS, see ./make.
(define %host-features
  (list
   (feature-host-info
    #:host-name "guix"
    #:timezone  "Europe/Paris"
    #:locale "fr_FR.utf8")
   (feature-hidpi)))

(define %machines
  (list
   (machine
    (name "Precision 3571")
    (efi "/dev/nvme0n1p1")
    (swap "/dev/nvme0n1p2")
    (uuid-mapped "86106e76-c07f-441a-a515-06559c617065")
    (firmware (list linux-firmware))
    (features %host-features))
   (machine
    (name "20AMS6GD00")
    (efi "/dev/sda1")
    (swap "/dev/sda2")
    (uuid-mapped "a9319ee9-f216-4cad-bfa5-99a24a576562")
    (features %host-features))
   (machine
    (name "2325K55")
    (efi "/dev/sda1")
    (swap "/dev/sda2")
    (uuid-mapped "1e7cef7b-c4dc-42d9-802e-71a50a00c20b")
    (firmware (list iwlwifi-firmware))
    (features %host-features))))


;; Privacy without GNUPG: currently using age with ssh and git commit signing. ;; TODO more details later.
;; Tip: sign outside git with ssh: `ssh-keygen -Y sign -n "file" -f /.ssh/id_ed25519_sk < "${file_to_sign}" > "${file_to_sign}.asc"'

(define %user-features
  (list
   (feature-age
    #:age-ssh-key (find-home "~/.ssh/id_encrypt"))
   ((@(rde features security-token) feature-security-token))
   (feature-password-store
    #:remote-password-store-url "git@git.sr.ht:~ngraves/pass"
    #:default-pass-prompt? #t)
   (feature-user-info
    #:user-name "graves"
    #:full-name "Nicolas Graves"
    #:email "ngraves@ngraves.fr"
    #:user-initial-password-hash
    "$6$zds6yx8zRvS3Oo7L$yM0SRwidUNhq3bsaqpvbizCQKhpJWqr5eWtzOQJYzH5jshGygfGn/apoZL6zVXYc5Yr9/TguaCRu9Sk5lEyKE0"
    #:emacs-advanced-user? #t)
   (feature-keyboard
    #:keyboard-layout
    (keyboard-layout "fr" "," #:options '("caps:escape")))))


;;; Window management
(define sound-message-in
  (origin
    (method url-fetch)
    (uri "https://raw.githubusercontent.com/KDE/oxygen-sounds/master/sounds/Oxygen-Im-Message-In.ogg")
    (sha256 (base32 "1w16z9wddhv4qq68xqwlikhqq2nnfzq31cn07k3wmdynnvzvy3ah"))))

(define sound-message-out
  (origin
    (method url-fetch)
    (uri "https://raw.githubusercontent.com/KDE/oxygen-sounds/master/sounds/Oxygen-Im-Message-Out.ogg")
    (sha256 (base32 "1cdqazyflx5vfnaz1n1sfilrmhg2796spzaszjgjbcbrdwrhgx6c"))))

(define background
  (origin
    (method url-fetch)
    (uri "https://pour-un-reveil-ecologique.org/media/images/fond_pre.original.jpg")
    (file-name "fond_pre.jpg")
    (sha256 (base32 "03rn4fw9j31s7hl635q872hzxj4bj5m9hkjd4iqzl8z4lk0n9iiy"))))

(define %wm-features
  (list
   (feature-sway
    #:xwayland? #t
    #:extra-config
    `((bindsym
       --to-code
       (($mod+dollar exec makoctl dismiss --all)
        ($mod+exclam exec makoctl set-mode dnd)
        ($mod+Shift+exclam exec makoctl set-mode default)
        ;; ($mod+w exec chromium --remote-debugging-port=9222)
        ;; ($mod+Shift+w exec chromium --incognito --remote-debugging-port=9222)
        ($mod+m exec ~/.local/bin/playm)
        ($mod+Shift+m exec killall mpv)))

      (exec wlsunset -l 48.86 -L 2.35 -T 6500 -t 3000)
      (exec mako)

      (workspace_auto_back_and_forth yes)
      (focus_follows_mouse no)
      (smart_borders on)
      (title_align center)

      (output * bg ,background fill)

      (bindsym --to-code --no-repeat
               $mod+twosuperior exec
               ,(file-append (@ (gnu packages linux) pipewire)
                             "/bin/pw-cat -p ")
               ,sound-message-in & ";"
               exec nerd-dictation begin
               --vosk-model-dir
               ,(file-append
                 (package
                   (name "vosk-model-fr")
                   (version "0.22")
                   (source
                    (origin
                      (method url-fetch/zipbomb)
                      (uri (string-append
                            "https://alphacephei.com/vosk/models/" name "-" version ".zip"))
                      (sha256
                       (base32 "0ihy93n6m5v9q22ky2hs1yvavsck3l592ppgdkp9v7qvxbjk8v5j"))))
                   (build-system (@(guix build-system copy) copy-build-system))
                   (arguments '(#:substitutable? #f))
                   (home-page "https://alphacephei.com/vosk/models")
                   (synopsis "French model for vosk")
                   (description "French model for vosk")
                   (license (@(guix licenses) asl2.0)))
                 "/vosk-model-fr-0.22/")
               --timeout 3 --delay-exit 2
               --punctuate-from-previous-timeout 10
               --full-sentence)

      (bindsym --to-code --release
               $mod+twosuperior exec
               ,(file-append (@ (gnu packages linux) pipewire)
                             "/bin/pw-cat -p ")
               ,sound-message-out && nerd-dictation end)

      (for_window "[app_id=\"^.*\"]" inhibit_idle fullscreen)
      (for_window
       "[title=\"^(?:Open|Save) (?:File|Folder|As).*\"]"
       floating enable, resize set width 70 ppt height 70 ppt)

      ;; (bindswitch --reload --locked lid:on exec /run/setuid-programs/swaylock)
      ;; (bindsym $mod+Shift+o ,#~"[floating]" kill)
      (input type:touchpad
             ;; TODO: Move it to feature-sway or feature-mouse?
             ( ;; (natural_scroll enabled)
              (tap enabled)))))
   (feature-sway-run-on-tty
    #:sway-tty-number 1)
   (feature-sway-screenshot
    #:screenshot-key 'F10)
   (feature-waybar
    #:waybar-modules
    (list
     ;; (waybar-sway-workspaces)
     ;; (waybar-sway-window)
     (waybar-tray)
     (waybar-idle-inhibitor)
     (waybar-microphone)
     (waybar-volume)
     (waybar-temperature)
     (waybar-sway-language)
     (waybar-battery #:intense? #f)
     (waybar-clock))
    #:base16-css
    (let* ((commit "d2f943b1abb9c9f295e4c6760b7bdfc2125171d2")
           (name "base16-default-dark.css"))
      (origin
        (method url-fetch)
        (uri
         (string-append
          "https://raw.githubusercontent.com/mnussbaum/base16-waybar/"
          commit "/colors/" name))
        (sha256
         (base32 "1dncxqgf7zsk39bbvrlnh89lsgr2fnvq5l50xvmpnibk764bz0jb")))))
   (feature-swayidle)
   (feature-swaylock
    #:swaylock (@ (gnu packages wm) swaylock-effects)
    #:extra-config
    `((clock)
      ,#~(string-append
          "image="
          #$(origin
              (method url-fetch)
              (uri "https://pour-un-reveil-ecologique.org/media/images/fond_lock_pre.original.jpg")
              (file-name "fond_lock_pre.jpg")
              (sha256
               (base32 "1cyvaj0yvy6zvzy9yf1z6i629rwjcq3dni01phb599sp4n2cpa8g"))))))
   (feature-swaynotificationcenter)))


;;; Mail
(define %mail-list
  (let ((passdir (find-home "~/.local/var/lib/password-store")))
    (append
     (list "ngraves@ngraves.fr") ;ensuring primary_email
     (delete "ngraves@ngraves.fr"
             (map (lambda file
                    (string-drop
                     (string-drop-right (car file) (string-length ".age"))
                     (+ 1 (string-length passdir))))
                  (find-files passdir "@[-a-z\\.]+\\.[a-z]{2,3}\\.age$"))))))

(define (id->type id)
  (cond
   ((string=? id "neleves") 'enpc)
   ((string=? id "ngmx") 'gmx-fr)
   ((string=? id "ngmail") 'gmail)
   ((string=? id "epour-un-reveil-ecologique") 'ovh-pro2-fr)
   (#t 'ovh)))

(define (user->id user)
  (string-append
   (string-take user 1)
   (car (string-split (car (cdr (string-split user #\@))) #\.))))

(define* (single-mail-acc user)
  "Make a simple mail-account with ovh type by default."
  (let* ((id_ (user->id user)))
    (list
     (mail-account
      (id (string->symbol id_))
      (fqda user)
      (type (id->type id_))
      (pass-cmd (string-append "passage show " user " | head -1"))))))

(define %msmtp-provider-settings
  (acons 'enpc '((host . "boyer2.enpc.fr")
                 (port . 465)
                 (tls_starttls . off))
         %default-msmtp-provider-settings))

(define* (mail-lst id fqda urls)
  "Make a simple mailing-list."
  (mailing-list
   (id   id)
   (fqda fqda)
   (config (l2md-repo
            (name (symbol->string id))
            (urls urls)))))

(define enpc-isync-settings
  (generate-isync-serializer
   "messagerie.enpc.fr"
   (@@ (rde features mail) gandi-folder-mapping)
   #:cipher-string 'DEFAULT@SECLEVEL=1
   #:pipeline-depth 1))

(define %isync-serializers
  (acons 'enpc enpc-isync-settings
         %default-isync-serializers))

(define %isync-global-settings
  `((Create Near)
    (Expunge Both)
    (SyncState *)
    (MaxMessages 0)
    (ExpireUnread no)
    ,#~""))

(define %mail-features
  (list
   (feature-mail-settings
    #:mail-accounts
    (append-map single-mail-acc %mail-list)
    #:mail-directory-fn (const (string-append (getenv "XDG_STATE_HOME") "/mail"))
    #:mailing-lists (list (mail-lst 'guix-devel "guix-devel@gnu.org"
                                    '("https://yhetil.org/guix-devel/0"))
                          (mail-lst 'guix-bugs "guix-bugs@gnu.org"
                                    '("https://yhetil.org/guix-bugs/0"))
                          (mail-lst 'guix-patches "guix-patches@gnu.org"
                                    '("https://yhetil.org/guix-patches/1"))))
   (feature-msmtp
    #:msmtp-provider-settings %msmtp-provider-settings)
   (feature-isync
    #:mail-account-ids
    (append-map
     (lambda (x) (list (string->symbol (user->id x)))) %mail-list)
    #:isync-global-settings %isync-global-settings
    #:isync-serializers %isync-serializers
    #:isync-verbose #t)
   (feature-notmuch)
   (feature-l2md)))


;;; SSH
(define-public (ssh-config id)
  (let* ((port
          (open-input-pipe
           (string-append "passage show ssh/ssh_" id " 2>/dev/null")))
         (key (read-line port))
         (ssh-user
          (when (string=? (read-delimited " " port) "Username:")
            (read-line port)))
         (uri
          (when (string=? (read-delimited " " port) "URI:")
            (read-line port)))
         (ssh-port
          (string->number
           (when (string=? (read-delimited " " port) "Port:")
             (read-line port))))
         (hostkey
          (when (string=? (read-delimited " " port) "HostKey:")
            (read-line port))))
    (close-pipe port)
    (openssh-host
     (name id)
     (host-name uri)
     (identity-file (string-append "~/.ssh/" key))
     (port ssh-port)
     (user ssh-user))))

(define %ssh-feature
  (feature-ssh
   #:ssh-agent? #t
   #:ssh-configuration
   (home-openssh-configuration
    (add-keys-to-agent "yes")
    (hosts (list (ssh-config "inari")
                 (ssh-config "pre_site"))))
   #:ssh-add-keys '("~/.ssh/id_sign")))


;;; Emacs
;; TODO Find a way to clarify current organization with much less text, more code.
;; PARA method (https://fortelabs.com/blog/para/) with directories spheres/resources/projects/archives.
;;             with resources managed with: see ./hooks/git-biblio-prepare-commit-msg
;; WANTED dynamic org-roam agenda (https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html)
;;
(define %org-agenda-custom-commands
  ``((,(kbd "C-d") "Agenda for the day"
      ((agenda
        ""
        ((org-agenda-span 1)
         (org-agenda-scheduled-leaders '("" "Sched.%2dx: "))
         (org-agenda-block-separator nil)
         (org-agenda-entry-types '(:scheduled :timestamp :sexp))
         (org-scheduled-past-days 0)
         ;; We don't need the `org-agenda-date-today'
         ;; highlight because that only has a practical
         ;; utility in multi-day views.
         (org-agenda-day-face-function (lambda (date) #'org-agenda-date))
         ;; (org-agenda-skip-function
         ;;  '(org-agenda-skip-entry-if 'todo '("NEXT")))
         (org-agenda-format-date "%A %-e %B %Y")
         (org-agenda-overriding-header "\nAgenda for the day\n")))
       (todo
        "NEXT"
        ((org-agenda-block-separator nil)
         (org-agenda-overriding-header "\nCurrent Tasks\n")))))
     (,(kbd "C-o") "Overview"
      ;; TODO: Add A priority to the top.
      ((tags-todo "+PRIORITY=\"A\""
                  ((org-agenda-block-separator nil)
                   (org-agenda-overriding-header "\nHigh Priority\n")))
       (tags-todo "+manage"
                  ((org-agenda-block-separator nil)
                   (org-agenda-overriding-header "\nBe a good manager\n")))
       (tags-todo "+followup"
                  ((org-agenda-block-separator nil)
                   (org-agenda-overriding-header "\nSomeone needs my follow up\n")))
       (agenda
        ""
        ((org-agenda-time-grid nil)
         (org-agenda-start-on-weekday nil)
         (org-agenda-start-day "+1d")
         (org-agenda-span 14)
         (org-agenda-show-all-dates nil)
         (org-agenda-time-grid nil)
         (org-deadline-warning-days 0)
         (org-agenda-block-separator nil)
         (org-agenda-entry-types '(:deadline))
         (org-agenda-skip-function '(org-agenda-skip-entry-if 'done))
         (org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n")))
       (tags-todo "+organize"
                  ((org-agenda-block-separator nil)
                   (org-agenda-overriding-header "\nOrganize even better\n")))
       (tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
                  ((org-agenda-block-separator nil)
                   (org-agenda-overriding-header "\nLow Effort / Batchable Tasks\n")
                   (org-agenda-max-todos 20)
                   (org-agenda-files org-agenda-files)))
       (agenda
        "*"
        ((org-agenda-block-separator nil)
         (org-agenda-span 14)
         (org-agenda-overriding-header "\nAgenda\n")))
       ))
     (,(kbd "C-r") "Review"
      ((agenda "" ((org-deadline-warning-days 7)))
       (todo "TODO"
             ((org-agenda-overriding-header "Agenda Tasks")
              (org-agenda-files (vulpea-project-files))
              (org-agenda-text-search-extra-files nil)))
       (todo "WAIT"
             ((org-agenda-overriding-header "Tasks waiting for someone else")))
       (alltodo
        ""
        ((org-agenda-block-separator nil)
         (org-agenda-skip-function '(or (org-agenda-skip-if nil '(scheduled deadline))))
         (org-agenda-overriding-header "\nBacklog\n")))))))

(define %extra-init-el
  `(;; .dir-locals.el management.
    ;; use rde style (buggy with eval).
    (dir-locals-read-from-dir "/home/graves/spheres/info/rde")
    (dir-locals-set-directory-class "/home/graves/spheres/info/dots"
                                    '/home/graves/spheres/info/rde)
    (dir-locals-set-class-variables
     '/home/graves/spheres/info/dots
     '((nil . ((eval . (progn
               (unless (boundp 'geiser-guile-load-path)
                     (defvar geiser-guile-load-path '()))
                   (make-local-variable 'geiser-guile-load-path)
                   (add-to-list 'geiser-guile-load-path "/home/graves/spheres/info/nonguix")
                   (add-to-list 'geiser-guile-load-path "/home/graves/spheres/info/rde/src")
                   (add-to-list 'geiser-guile-load-path "/home/graves/spheres/info/guix")))))))
    (dir-locals-set-directory-class "/home/graves/spheres/info/dots"
                                    '/home/graves/spheres/info/dots)

    (defun format-xml ()
      "Format XML files using libxml2."
      (interactive)
      (shell-command-on-region
       1 (point-max)
       ,(file-append (@(gnu packages xml) libxml2) "/bin/xmllint --format -")
       (current-buffer) t))
    ;; pomodoro
    (eval-when-compile (require 'org-pomodoro))
    ;; clocking
    (setq org-clock-persist 'history)
    (org-clock-persistence-insinuate)
    ;; clocking in the task when setting a timer on a task
    (add-hook 'org-timer-set-hook 'org-clock-in)

    ;; origami
    (eval-when-compile (require 'origami))

    ;; html email and elfeed
    (setq shr-current-font "Iosevka")

    ;; help in learning keybindings
    (global-set-key (kbd "s-?") 'embark-bindings)

    ;; bibliography
    (setq citar-library-file-extensions '("pdf.lz"))
    (require 'f)
    (setq biblio-bibtex-use-autokey t)
    (setq bibtex-autokey-year-title-separator "_")
    (setq bibtex-autokey-year-length 4)
    (defun refresh-gen-biblio ()
      "Regenerates the generated gen.bib file based on the list in dois.txt."
      (interactive)
      (with-temp-file "/tmp/retrieved_dois.txt"
                      (maphash
                       (lambda (k v)
                         (insert
                          (concat (cdr (car v)) "\n")))
                       (parsebib-parse "~/resources/gen.bib"
                                       :fields '("doi"))))
      (with-temp-buffer
       (let ((biblio-synchronous t))
         (mapcar (lambda (x) (biblio-doi-insert-bibtex x))
                 (cl-set-exclusive-or
                  (s-split "\n" (f-read "~/resources/dois.txt") t)
                  (s-split "\n" (f-read "/tmp/retrieved_dois.txt") t)
                  :test 'string-equal-ignore-case)))
       (append-to-file nil nil "~/resources/gen.bib")))))

(define %additional-elisp-packages
  (append (list (@(rde packages emacs-xyz) emacs-git-email-latest)
                (@(rde packages emacs-xyz) emacs-dirvish-latest)
                (package
                  (inherit (@(gnu packages emacs-xyz) emacs-biblio))
                  (arguments
                   (list
                    #:phases
                    #~(modify-phases %standard-phases
                        (add-after 'unpack 'configure-const
                          (lambda _
                            (substitute* "biblio-doi.el"
                              (("text\\/bibliography;style=bibtex, application\\/x-bibtex")
                               "application/x-bibtex")))))))))
          (strings->packages
           "emacs-org-roam-ui"
           "emacs-hl-todo"
           "emacs-consult-dir"
           "emacs-restart-emacs"
           ;; "emacs-ibrowse"
           "emacs-mini-frame"
           "emacs-link-hint"
           "emacs-forge"
           "emacs-consult-org-roam"
           "emacs-origami-el"
           "emacs-emojify"
           "emacs-wgrep"
           "emacs-flycheck-package"
           "python-lsp-server"
           "emacs-org-pomodoro")))

(define %emacs-features
  (list
   ((@(rde features emacs) feature-emacs)
    #:default-application-launcher? #t)
   (feature
    (name 'emacs-custom)
    (home-services-getter
     (lambda (_)
       (list
        (simple-service
         'emacs-extensions
         home-emacs-service-type
         (home-emacs-extension
          (init-el %extra-init-el)
          (elisp-packages %additional-elisp-packages)))))))
   (feature-emacs-message)
   (feature-emacs-appearance)
   (feature-emacs-modus-themes
    #:deuteranopia? #f)
   (feature-emacs-completion
    #:mini-frame? #f)
   (feature-emacs-corfu)
   (feature-emacs-vertico)
   (feature-emacs-project)
   (feature-emacs-perspective)
   (feature-emacs-input-methods)
   (feature-emacs-which-key)
   (feature-emacs-pdf-tools)
   (feature-emacs-comint)
   (feature-emacs-webpaste)
   (feature-emacs-help)
   (feature-emacs-all-the-icons)
   (feature-emacs-debbugs)
   (feature-emacs-flymake)
   (feature-emacs-xref)
   (feature-emacs-info)
   (feature-emacs-popper)
   (feature-emacs-spelling
    #:spelling-program (@ (gnu packages hunspell) hunspell)
    #:spelling-dictionaries (strings->packages
                             "hunspell-dict-en"
                             "hunspell-dict-fr"))

   (feature-emacs-tramp)
   (feature-emacs-dired
    #:kill-when-opening-new-buffer? #t)
   (feature-emacs-dired-ranger)
   (feature-emacs-eshell)
   (feature-emacs-monocle
    #:olivetti-body-width 120)

   (feature-emacs-git)
   (feature-emacs-org
    #:org-directory "~"
    #:org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "|" "HOLD(h)")) ; waiting for someone to be ationable again
    #:org-tag-alist
    '((:startgroup)
      ;; Put mutually exclusive tags here
      (:endgroup)
      ("@home" . ?H)
      ("@work" . ?W)
      ("batch" . ?b) ; batchable task
      ("manage" . ?m) ; I'm responsible for the rythm of others
      ("organize" . ?o) ; better organization
      ("followup" . ?f))) ; someone is waiting on me to follow up

   (feature-emacs-org-agenda
    #:org-agenda-files '("~/spheres/life/journal")
    #:org-agenda-custom-commands %org-agenda-custom-commands)
   (feature-emacs-smartparens #:show-smartparens? #t)
   (feature-emacs-eglot)
   (feature-emacs-geiser)
   (feature-emacs-graphviz)
   (feature-emacs-guix
    #:guix-directory "/home/graves/spheres/info/guix")
   (feature-emacs-tempel #:default-templates? #t)

   (feature-emacs-evil)
   (feature-emacs-saving)
   (feature-emacs-elfeed #:elfeed-org-files '("~/resources/feeds.org"))
   (feature-emacs-org-protocol)
   (feature-emacs-org-roam
    #:org-roam-directory "~"
    #:org-roam-file-exclude-regexp
    '(list "^tmp/" "^\\." "^archives/" "^resources/files/" "^projects/src")
    #:org-roam-capture-templates
    '(("m" "main" plain "%?"
       :if-new (file+head "resources/main/${slug}.org"
                          "#+title: ${title}\n")
       :immediate-finish t
       :unnarrowed t))
    #:org-roam-dailies-directory "spheres/life/journal")
   (feature-emacs-citar
    #:citar-library-paths (list "~/resources/files/library")
    #:citar-notes-paths (list "~/resources/references")
    #:global-bibliography (list "~/resources/biblio.bib" "~/resources/gen.bib"))

   (feature-emacs-eval-in-repl #:repl-placement 'right)
   (feature-go)
   (feature-python #:black? #t)

   (feature-emacs-elisp)
   (feature-emacs-power-menu)
   (feature-emacs-shell)
   (feature-emacs-flycheck)))


;;; Main features

(define %main-features
  (append
   (list
    (feature-postgresql
     #:postgresql-roles
     (list ((@(gnu services databases) postgresql-role) (name "manifesto"))))

    (feature-desktop-services)
    (feature-backlight #:step 5)
    (feature-pipewire)
    (feature-networking)

    (feature-fonts #:default-font-size 14)
    (feature-alacritty
     #:default-terminal? #f
     #:backup-terminal? #t
     #:software-rendering? #t)
    (feature-vterm)
    (feature-zsh #:enable-zsh-autosuggestions? #t)
    (feature-bash)
    ;; ((@(rde features bittorrent) feature-transmission))

    (feature-compile)
    (feature-direnv)

    ((@(rde features version-control) feature-git)
     #:sign-commits? #t
     #:git-sign-key
     "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINd9BmbuU3HS6pbCzCe1IZGxaHHDJERXpQRZZiRkfL3a"
     #:git-send-email? #t)

    (feature-ledger)
    (feature-markdown)
    ;; (feature-tex
    ;; #:extra-tex-packages (strings->packages "texlive"))
    (feature-mpv)
    (feature-youtube-dl)
    (feature-imv)

    ((@(rde features tmux) feature-tmux))
    ((@(rde features docker) feature-docker))
    ;; (feature-ungoogled-chromium
    ;; #:default-browser? #t)
    (feature-nyxt)

    ((@(rde features xdg) feature-xdg)
     #:xdg-user-directories-configuration
     ((@(gnu home services xdg) home-xdg-user-directories-configuration)
      (download "~/tmp")
      (videos "~/archives/videos")
      (music "~/archives/music")
      (pictures "~/archives/img")
      (documents "~/resources")
      (desktop "~")
      (publicshare "~")
      (templates "~")))

    (feature-base-packages
     #:home-packages
     (cons*
      (package
        (inherit (@ (gnu packages machine-learning) llama-cpp))
        (name "llama")
        (native-inputs '())
        (inputs
         (list (@ (gnu packages bash) bash)
               (@ (gnu packages machine-learning) llama-cpp)))
        (build-system (@ (guix build-system trivial) trivial-build-system))
        (arguments
         (list
          #:modules '((guix build utils))
          #:builder
          #~(begin
              (use-modules (guix build utils))
              (let* ((exe (string-append #$output "/bin/interactive-llama")))

                (mkdir-p (dirname exe))

                (call-with-output-file exe
                  (lambda (port)
                    (format port "#!~a

~a -m ~a \
--color \
--ctx_size 2048 \
-n -1 \
-ins -b 256 \
--top_k 10000 \
--temp 0.2 \
--repeat_penalty 1.1 \
-t 8"
                            #$(file-append (this-package-input "bash") "/bin/bash")
                            #$(file-append (this-package-input "llama-cpp") "/bin/llama")
                            #$(let ((model "llama-2-13b-chat.ggmlv3.q5_1.bin"))
                                (origin
                                  (method url-fetch)
                                  (uri
                                   (string-append
                                    "https://huggingface.co/TheBloke/Llama-2-13B-chat-GGML/resolve/main/"
                                    model))
                                  (sha256
                                   (base32 "0xpy7mz52pp48jw20cv24p02dsyn0rsjxj4wjp3j6hrnbb6vxncp")))))))
                (chmod exe #o555))))))
      (strings->packages
       "hicolor-icon-theme" "adwaita-icon-theme" ;; themes
       "alsa-utils"  ;; sound
       "bluez"  ;; bluetooth
       "ffmpeg"  ;; video
       "rsync" "zip" "libreoffice" "thunar"  ;; documents
       "libnotify" "wev" "wlsunset" ;; wayland
       "recutils" "curl"  ;; utils
       ;; other
       "btrbk"
       "atool" ;; provides generic extract tool aunpack
       ;; "nerd-dictation-sox-wtype"
       "git-lfs"
       "pinentry-qt"))))
   %wm-features
   %emacs-features
   %mail-features
   (list %ssh-feature)))


;;; rde-config and helpers for generating home-environment and
;;; operating-system records.

(define %config
  (rde-config
   (features
    (append
     (list %nonguix-feature) ;; defined in make.
     %user-features
     %main-features
     (get-hardware-features %machines))))) ;; defined in make.


;;; Installation

;; More info : https://guix.gnu.org/manual/en/html_node/System-Installation.html
;;             https://wiki.systemcrafters.cc/guix/nonguix-installation-guide

;; Building the installation image: `./make live-install'

;; Sending to USB stick: `sudo dd if=/gnu/store/{sha256}-disk-image of=/dev/sdX bs=1M status=progress'

;; Boot --> shell process --> Wifi: `rfkill unblock all && nmtui'

;; Setup partitions : `fdisk /dev/sda' (see https://guix.gnu.org/manual/en/guix.html#Disk-Partitioning)

;; Setup encryption :
;; mkfs.vfat -F32 /dev/<EFI partition>
;; cryptsetup luksFormat /dev/<root partition>
;; cryptsetup open --type luks /dev/<root partition> enc
;; mkfs.btrfs /dev/mapper/enc
;; mount LABEL=enc /mnt # or mount -t btrfs /dev/mapper/enc /mnt
;; btrfs subvolume create /mnt/root
;; btrfs subvolume create /mnt/boot
;; btrfs subvolume create /mnt/home
;; btrfs subvolume create /mnt/store
;; btrfs subvolume create /mnt/data
;; btrfs subvolume create /mnt/log
;; btrfs subvolume create /mnt/lib
;; btrfs subvolume create /mnt/guix
;; btrfs subvolume create /mnt/etc/NetworkManager
;; btrfs subvolume create /mnt/btrbk_snapshots
;; btrfs subvolume create /mnt/swap
;; umount /mnt
;; mount -o subvol=root /dev/mapper/enc /mnt
;; mkdir -p /mnt/home
;; mkdir -p /mnt/gnu/store
;; mkdir -p /mnt/data
;; mkdir -p /mnt/var/log
;; mkdir -p /mnt/var/lib
;; mkdir -p /mnt/var/guix
;; mkdir -p /mnt/etc/NetworkManager
;; mkdir -p /mnt/btrbk_snapshots
;; mkdir -p /mnt/boot
;; mount -o compress=zstd,discard,space_cache=v2,subvol=home /dev/mapper/enc /mnt/home
;; mount -o compress=zstd,discard,space_cache=v2,subvol=store /dev/mapper/enc /mnt/gnu/store
;; mount -o compress=zstd,discard,space_cache=v2,subvol=data /dev/mapper/enc /mnt/data
;; mount -o compress=zstd,discard,space_cache=v2,subvol=log /dev/mapper/enc /mnt/var/log
;; mount -o compress=zstd,discard,space_cache=v2,subvol=lib /dev/mapper/enc /mnt/var/lib
;; mount -o compress=zstd,discard,space_cache=v2,subvol=guix /dev/mapper/enc /mnt/var/guix
;; mount -o compress=zstd,discard,space_cache=v2,subvol=etc/NetworkManager /dev/mapper/enc /mnt/etc/NetworkManager
;; mount -o compress=zstd,discard,space_cache=v2,subvol=btrbk_snapshots /dev/mapper/enc /mnt/btrbk_snapshots
;; mount -o compress=zstd,discard,space_cache=v2,subvol=boot /dev/mapper/enc /mnt/boot
;; mkdir -p /mnt/boot/efi
;; mount /dev/<EFI partition> /mnt/boot/efi
;; btrfs filesystem mkswapfile --size 4g --uuid clear /mnt/swap/swapfile
;; mount -o nodatacow,nodatasum,subvol=swap /dev/mapper/enc /mnt/swap
;; swapon /mnt/swap/swapfile

;; Setup installation environment : `herd start cow-store /mnt'

;; Pull: `guix pull -C /etc/channels && hash guix' ; TODO Update

;; Find encrypted partition UUIDs for configuration: `cryptsetup luksUUID /dev/<root partition>'

;; Init installation: `guix system -L ~/.dotfiles/.config/guix/systems init path/to/config /mnt' ; TODO Update

;; Reboot --> `passwd' --> `passwd <username>'

;; Adapt configuration until you reach what you want.


;;; Future ideas
;; - learning panel with org-roam/org-drill
;; - proper clocking / pomodoro
;; - email workflow integration
;; - larbs: replicate dmenumount / dmenuumount functionality
;; - larbs: replicate linkhandler / menuhandler functionality
;;          embark does a lot. tweak ffap-menu, link-hint and embark-open-externally.
;; - larbs: mail notification / inbox
;; - larbs: sb-nettraf / or vnstat?
;; - age integration or sequoia.
;; - OVH email aliases.
;; - maybe switch to programmer-beop: `https://github.com/luxcem/programmer-beop'
;; - vterm evil-mode integration. see in commit 0981704751f201eaf4e852421ae8fd7d1ffa7dd9

;;; Currently abandonned:
;; - system-connection services. see commit log.

;; Local Variables:
;; compilation-arguments: ("./make all" t nil nil)
;; End:
