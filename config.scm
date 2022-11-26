;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>
;; Channels are managed through the script channels.tmpl.scm and the makefile.


;;; Hardware/host file systems

(use-modules
 (rde features base)
 (guix gexp)
 (guix channels)
 (ice-9 match)
 (ice-9 pretty-print)
 (ice-9 rdelim)
 (ice-9 popen)
 (ice-9 string-fun)
 (gnu system)
 (gnu packages)
 (gnu system file-systems)
 (gnu system mapped-devices))

(define devices
  '(("Precision-3571" .
     ((efi         . /dev/nvme0n1p1)
      (swap        . /dev/nvme0n1p2)
      (uuid-mapped . 86106e76-c07f-441a-a515-06559c617065)
      (firmware    . (list linux-firmware))))
    ("20AMS6GD00" .
     ((efi         . /dev/sda1)
      (swap        . /dev/sda2)
      (uuid-mapped . a9319ee9-f216-4cad-bfa5-99a24a576562)))
    ("2325K55" .
     ((efi         . /dev/sda1)
      (swap        . /dev/sda2)
      (uuid-mapped . 1e7cef7b-c4dc-42d9-802e-71a50a00c20b)
      (firmware    . (list iwlwifi-firmware))))))

(define (getdevicename)
  "This function looks up the value of the current device name."
  (string-replace-substring
  (call-with-input-file "/sys/devices/virtual/dmi/id/product_name"
    (lambda (port) (read-line port))) " " "-" ))

(define (lookup var)
  "This function looks up in devices the value of var on the current device."
  (let* ((content (cdr (assoc var (cdr (assoc (getdevicename) devices))))))
    (if (eq? var 'firmware)
        (map (lambda x (specification->package (symbol->string (car x))))
             (cdr content))
        (symbol->string content))))

(define %mapped-device
  (mapped-device
   (source (uuid (lookup 'uuid-mapped)))
   (targets (list "enc"))
   (type luks-device-mapping)))

(define file-systems
  (append
   (map
    (match-lambda
      ((subvol . mount-point)
       (file-system
         (type "btrfs")
         (device "/dev/mapper/enc")
         (mount-point mount-point)
         (options
          (format
           #f "autodefrag,compress=zstd,ssd_spread,space_cache=v2,subvol=~a" subvol))
         (dependencies (list %mapped-device)))))
    '((root . "/")
      (store  . "/gnu/store")
      (home . "/home")
      (data . "/data")
      (snap . "/snap")
      (boot . "/boot")
      (log  . "/var/log")))
   (list (file-system
           (mount-point "/boot/efi")
           (type "vfat")
           (device (lookup 'efi))))))


;;; Hardware/host specifis features

;; System modules
(use-modules
   (rde features system)
   (rde features base)
   (gnu system file-systems)
   (nongnu system linux-initrd)
   (nongnu packages linux))

(define live-file-systems
  (list (file-system
          (mount-point "/")
          (device (file-system-label "Guix_image"))
          (type "ext4"))
        (file-system
          (mount-point "/tmp")
          (device "none")
          (type "tmpfs")
          (check? #f))))

(define %host-features
  (list
   (feature-host-info
    #:host-name "guix"
    #:timezone  "Europe/Paris")
   (feature-bootloader)
   (feature-file-systems
    #:mapped-devices (list %mapped-device)
    #:swap-devices (list (swap-space (target (lookup 'swap))))
    #:file-systems file-systems)
   (feature-kernel
    #:kernel linux
    #:initrd microcode-initrd
    #:initrd-modules
    (append (list "vmd") (@ (gnu system linux-initrd) %base-initrd-modules))
    #:kernel-arguments
    (append (list "quiet" "rootfstype=btrfs") %default-kernel-arguments)
    #:firmware (lookup 'firmware))
   (feature-hidpi)))


;;; User Features
(use-modules
 (gnu system keyboard)
 (gnu packages password-utils)
 (rde packages)
 (rde features privacy)
 (rde features security-token)
 (rde features keyboard)
 (rde features password-utils))

(define %user-features
  (list
   (feature-user-info
    #:user-name "graves"
    #:full-name "Nicolas Graves"
    #:email "ngraves@ngraves.fr"
    #:user-initial-password-hash "gaAxdKLOplpY2"  ;; (crypt "bob" "$6$abc")
    #:emacs-advanced-user? #t)
   (feature-pinentry)
   (feature-age
    #:age-ssh-key "$HOME/.ssh/id_encrypt")
   ;; (feature-gnupg
   ;; #:gpg-ssh-agent? #t
   ;; #:ssh-keys
   ;; '(("4B8C7C409D8E286BAF9F1B075181FFE6E0AF7249")
   ;; ("748668172FB0CE88407F006E6ABD649DDD3EF2DD")
   ;; ("F204255D0F694AC6CEC585EFC21FFE27298B9D92"))
   ;; #:gpg-primary-key "3F61A23D53B5B118")
   (feature-security-token)

   (feature-password-store
    #:remote-password-store-url "git@git.sr.ht:~ngraves/pass"
    #:default-pass-prompt? #t)

   (feature-keyboard
    #:keyboard-layout
    (keyboard-layout "fr" "," #:options '("caps:escape")))))


;;; USB install
(use-modules
 (rde features)
 (guix gexp)
 (gnu packages)
 (gnu system install)
 (gnu packages fonts)
 (gnu services)
 (gnu services networking)
 (srfi srfi-26))

(define live-install
  (rde-config
   (initial-os installation-os)
   (features
    (append
     %user-features
     (list
      (feature-file-systems
       #:file-systems live-file-systems)
      (feature-kernel
       #:kernel linux
       #:firmware (list linux-firmware))
      (feature-base-packages
       #:system-packages
       (append
        (strings->packages
         "ripgrep" "vim" "git" "emacs-no-x" "zip" "unzip"
         "exfat-utils" "fuse-exfat" "ntfs-3g" "grub" "make" "glibc"
         "network-manager" "nss-certs" "curl"
         "fontconfig" "font-dejavu" "font-gnu-unifont" "font-terminus")
        %base-packages-disk-utilities
        %base-packages))
      (feature-base-services
       #:guix-substitute-urls
       (append (list "https://substitutes.nonguix.org")
               (@ (guix store) %default-substitute-urls))
       #:guix-authorized-keys
       (append (list (local-file "./config/keys/nonguix.pub"))
              (@ (gnu services base) %default-authorized-guix-keys))
       #:base-system-services
       (let* ((path "/share/consolefonts/ter-132n")
              (font #~(string-append #$font-terminus #$path))
              (ttys '("tty1" "tty2" "tty3" "tty4" "tty5" "tty6")))
         (append
          (list
           (simple-service
            'channels-and-sources
            etc-service-type
            `(("channels.scm" ,(local-file "./channels.scm"))
              ("guix-sources" ,(local-file "../guix" #:recursive? #t))
              ("nonguix-sources" ,(local-file "../nonguix" #:recursive? #t))
              ("rde-sources" ,(local-file "../rde" #:recursive? #t))
              ;;("dotfiles-sources" ,(local-file  #:recursive? #t))
              ))
           (service network-manager-service-type))
          (modify-services ((@@ (gnu system install) %installation-services))
            (console-font-service-type
             config =>
             (map (cut cons <> font) ttys))
            (delete connman-service-type)
            (delete openssh-service-type))))))))))

(define live-usb
  (rde-config-operating-system live-install))


;;; Window management
(use-modules
 (features wm)
 (rde features wm)
 (gnu packages gtk)
 (files))

(define %window-management-features
  (list
   (ng-feature-sway
    #:xwayland? #t
    #:extra-config
    `((bindsym
       --to-code
       (($mod+dollar exec makoctl dismiss --all)
        ($mod+exclam exec makoctl set-mode dnd)
        ($mod+Shift+exclam exec makoctl set-mode default)
        ($mod+w exec chromium --remote-debugging-port=9222)
        ($mod+Shift+w exec chromium --incognito --remote-debugging-port=9222)
        ($mod+m exec ~/.local/bin/playm)
        ($mod+Shift+m exec killall mpv)))

      (exec wlsunset -l 48.86 -L 2.35 -T 6500 -t 3000)
      (exec mako)

      (workspace_auto_back_and_forth yes)
      (focus_follows_mouse no)
      (smart_borders on)
      (title_align center)

      (output * bg ,(file-append bg "/fond_pre.jpg") fill)
      (input "9011:26214:ydotoold_virtual_device"
             ((xkb_layout "us")))

      (bindsym --to-code
               $mod+oe exec nerd-dictation begin
               --vosk-model-dir ,(file-append vosk-model-en-us "/vosk-model-en-us-0.22/")
               --timeout 3  --delay-exit 1.5
               --punctuate-from-previous-timeout 10
               --full-sentence)

      (bindsym --to-code $mod+Shift+oe exec nerd-dictation end)

      (for_window "[app_id=\"^.*\"]" inhibit_idle fullscreen)
      (for_window
       "[title=\"^(?:Open|Save) (?:File|Folder|As).*\"]"
       floating enable, resize set width 70 ppt height 70 ppt)

      ;; (bindswitch --reload --locked lid:on exec /run/setuid-programs/swaylock)
      (exec nm-applet --indicator)

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
    (file-append base16-waybar "/colors/base16-default-dark.css"))
   (feature-swayidle)
   (feature-swaylock
    #:swaylock (@ (gnu packages wm) swaylock-effects)
    #:extra-config
    `((clock)
     ,#~(string-append "image=" #$bg-lock "/fond_lock_pre.jpg")))
   (feature-swayr)))


;;; Mail
(use-modules
 (rde features mail)
 (guix build utils)
 (srfi srfi-1)
 (gnu packages mail))

(define %mail-list
  (let ((passdir (string-append (getenv "HOME") "/.local/var/lib/password-store")))
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

(define (my-mail-directory-fn config)
 (string-append (getenv "XDG_STATE_HOME") "/mail"))

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
    #:mail-directory-fn my-mail-directory-fn
    #:mailing-lists (list (mail-lst 'guix-devel "guix-devel@gnu.org"
                                    '("https://yhetil.org/guix-devel/0"))
                          (mail-lst 'guix-bugs "guix-bugs@gnu.org"
                                    '("https://yhetil.org/guix-bugs/0"))
                          (mail-lst 'guix-patches "guix-patches@gnu.org"
                                    '("https://yhetil.org/guix-patches/1"))))
   (feature-msmtp
    #:msmtp msmtp
    #:msmtp-provider-settings %msmtp-provider-settings)
   (feature-isync
    #:mail-account-ids
    (append-map
     (lambda (x) (list (string->symbol (user->id x)))) %mail-list)
    #:isync-global-settings %isync-global-settings
    #:isync-serializers %isync-serializers
    #:isync-verbose #t)
   (feature-notmuch)
   (feature-emacs-message)
   (feature-l2md)))


;;; SSH

(use-modules
  (gnu packages ssh)
  (rde features ssh)
  ;; (services ssh-utils)
  )

(define %ssh-feature
  (list
   (feature-ssh
    #:ssh openssh-sans-x
    #:ssh-agent? #t
    #:ssh-configuration
    (home-ssh-configuration
     (package openssh-sans-x)
     (toplevel-options
      '((add-keys-to-agent . #t)))
     ;; (toplevel-options
      ;; '((match . "host * exec \"gpg-connect-agent UPDATESTARTUPTTY /bye\"")))
     (user-known-hosts-file
      ;; This file is private, hidden, and writable on purpose.
      '("~/spheres/info/dots/config/ssh/known_hosts"
        "~/.ssh/my_known_hosts"))
     (default-host "*")
     (default-options
       '((add-keys-to-agent . #t)
         (address-family . "inet")))
     ;; (extra-config
      ;; `(,(car (ssh-config "inari"))
        ;; ,(car (ssh-config "pre_site"))
        ;; ,(car (ssh-config "pre_bitwarden"))))
     ))
    ))

(define ssh-files
  (list
   `(".ssh/id_rsa.pub" ,(local-file "config/keys/id_rsa.pub"))
   `(".ssh/id_ed25519.pub" ,(local-file "config/keys/id_ed25519.pub"))
   `(".ssh/id_rsa_git.pub" ,(local-file "config/keys/id_rsa_git.pub"))
   ;; `(".ssh/my_known_hosts"
     ;; ,(plain-file "my_known_hosts"
                  ;; (string-append
                   ;; (car (cdr (ssh-config "pre_site")))
                   ;; (car (cdr (ssh-config "pre_bitwarden")))
                   ;; (car (cdr (ssh-config "inari"))))))
   ))


;;; Emacs
(use-modules
 (features emacs)
 (rde features emacs)
 (rde features emacs-xyz)
 (rde packages emacs)
 (rde packages emacs-xyz)
 (packages emacs))

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
             ((org-agenda-overriding-header "Unprocessed Inbox Tasks")
              (org-agenda-files '("/home/graves/spheres/todo.org")))
             (org-agenda-text-search-extra-files nil))
       (todo "WAIT"
             ((org-agenda-overriding-header "Tasks waiting for someone else")))
       (alltodo
        ""
        ((org-agenda-block-separator nil)
         (org-agenda-skip-function '(or (org-agenda-skip-if nil '(scheduled deadline))))
         (org-agenda-overriding-header "\nBacklog\n")))))))

(define %emacs-features
  (list
   (feature-emacs
    #:extra-init-el
    `(;; using external programs sometimes requires having this variable set FIXED in feature-sway from rde
      (setenv "WAYLAND_DISPLAY"
              (car (directory-files (getenv "XDG_RUNTIME_DIR") nil "wayland-[0-9]$")))
      (defun format-xml ()
        "Format XML files using libxml2."
        (interactive)
        (shell-command-on-region 1 (point-max) "xmllint --format -" (current-buffer) t))
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

      ;; python org babel work
      (require 'eval-in-repl-python)
      (add-hook 'org-babel-post-tangle-hook
                (lambda ()
                  (let ((pyfilename
                         (string-replace "\\.org" "\\.py" buffer-file-name)))
                    (if (file-exists-p pyfilename)
                        (eir-python-shell-send-string
                         (org-file-contents pyfilename))))))

      ;; bibliography
      ;; adding post-stage-hook when magit-annex-add, useful for replacement of zotero
      (with-eval-after-load
       'magit
       (setq magit-post-stage-hook-commands
             (append '(magit-annex-add magit-annex-add-all)
                     magit-post-stage-hook-commands)))

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
                    :test (lambda (x y) (equal x y)))))
         (append-to-file nil nil "~/resources/gen.bib"))))
    #:additional-elisp-packages
    (append (list emacs-ol-notmuch
                  emacs-git-email-latest
                  (@ (packages emacs) emacs-biblio)
                  (@ (packages emacs) emacs-ibrowse))
            (strings->packages
             "emacs-hl-todo"
             "emacs-consult-dir"
             "emacs-dirvish"
             "emacs-restart-emacs"
             "emacs-app-launcher"
             "emacs-magit-annex"
             "emacs-mini-frame"
             "emacs-consult-org-roam"
             "emacs-origami-el"
             "emacs-emojify"
             "python-lsp-server"
             "emacs-org-pomodoro"))
    #:default-application-launcher? #f)
   (feature-emacs-appearance
    #:deuteranopia? #f)
   (feature-emacs-faces)
   (feature-emacs-completion
    #:mini-frame? #f)
   (feature-emacs-corfu)
   (feature-emacs-vertico)
   (feature-emacs-project)
   (feature-emacs-perspective)
   (feature-emacs-input-methods)
   (feature-emacs-which-key)
   (feature-emacs-pdf-tools)

   (feature-emacs-tramp)
   (feature-emacs-dired)
   (feature-emacs-eshell)
   (feature-emacs-monocle
    #:olivetti-body-width 120)

   ;; TODO: Revisit <https://en.wikipedia.org/wiki/Git-annex>
   (feature-emacs-git)
   ;; TODO: <https://www.labri.fr/perso/nrougier/GTD/index.html#table-of-contents>
   (feature-emacs-org
    #:org-directory "~/spheres"
    #:org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "|" "HOLD(h)"))
    #:org-tag-alist
    '((:startgroup)
      ;; Put mutually exclusive tags here
      (:endgroup)
      ("@home" . ?H)
      ("@work" . ?W)
      ("batch" . ?b)
      ("manage" . ?m)
      ("organize" . ?o)
      ("followup" . ?f)))

   (feature-emacs-org-agenda
    #:org-agenda-files '("~/spheres")
    #:org-agenda-custom-commands %org-agenda-custom-commands)
   (feature-emacs-smartparens
    #:show-smartparens? #t)
   (feature-emacs-eglot)
   (feature-emacs-geiser)
   (feature-emacs-guix
    #:guix-directory "/home/graves/spheres/info/guix")
   (feature-emacs-tempel
    #:default-templates? #t)

   ;; features I added myself
   (feature-emacs-evil
    #:stateful-keymaps? #t
    #:nerd-commenter? #t)
   (feature-emacs-saving)
   (feature-emacs-elfeed
    #:elfeed-org-files '("~/resources/feeds.org"))
   (feature-emacs-org-protocol)
   (feature-emacs-dired-hacks
    #:evil? #t)
   (feature-emacs-guix-development
    #:guix-load-path "/home/graves/spheres/info/guix"
    #:other-guile-load-paths (list "/home/graves/spheres/info/rde"))
   (feature-emacs-org-babel
    #:load-language-list
    (list "emacs-lisp" "python" "dot" "shell" "scheme")
    #:block-templates? #t)
   (feature-emacs-org-latex
    #:export-source-code? #t)
   (feature-emacs-org-roam
    #:org-roam-directory "~/resources"
    #:org-roam-capture-templates
    '(("m" "main" plain "%?"
       :if-new (file+head "main/${slug}.org"
                          "#+title: ${title}\n")
       :immediate-finish t
       :unnarrowed t)
      ("r" "reference" plain "%?"
       :if-new
       (file+head "references/${slug}.org"
                  "#+title: ${title}\n")
       :immediate-finish t
       :unnarrowed t)
      ("s" "Slipbox" entry  (file "resources/todo.org")
       "* %?\n"))
    #:use-node-types? #t
    #:org-roam-dailies-directory "../archives/journal")
   (feature-emacs-citar
    #:citar-library-paths (list "~/resources/files/library")
    #:citar-notes-paths (list "~/resources")
    #:global-bibliography (list "~/resources/biblio.bib" "~/resources/gen.bib"))
   (feature-emacs-eval-in-repl
    #:load-language-list
    (list "emacs-lisp" "python" "shell" "scheme")
    #:repl-placement "right"
    #:rely-on-geiser? #t)
   (feature-emacs-python)
   (feature-emacs-web-mode
    #:rainbow-mode? #t)
   ;; (feature-emacs-lispy)
   (feature-emacs-flycheck)))


;;; Main features
(use-modules
 (rde features linux)
 (rde features fontutils)
 (rde features terminals)
 (rde features shells)
 (rde features shellutils)
 (rde features version-control)
 (features xdisorg)
 (rde features markup)
 (rde features video)
 (rde features tmux)
 (rde features xdg)
 (gnu home services)
 (gnu home services xdg)
 (gnu home services shells)
 (gnu packages xdisorg)
 (packages xdisorg))

(define %main-features
  (append
   (list
    (feature-custom-services
     #:feature-name-prefix 'ixy
     #:system-services '()
     #:home-services
     (list
      (service
       home-xdg-configuration-files-service-type
       (list
        `("guix/channels.scm" ,(local-file "./channels.scm"))
        `("shell/aliasrc" ,(local-file "config/aliasrc"))
        `("wget/wgetrc" ,(plain-file "wgetrc" "hsts-file=~/.cache/wget-hsts\n"))))
      (service home-files-service-type
               `((".local/bin" ,(local-file "scripts" #:recursive? #t))))
      (simple-service
       'home-xdg-applications
       home-xdg-mime-applications-service-type
       (home-xdg-mime-applications-configuration
        ;; The imv entry is included in the package, but chromium was set as default.
        (default '((image/jpeg . imv.desktop)
                   (image/png . imv.desktop)))))
      (simple-service
       'extend-environment-variables
       home-environment-variables-service-type
       `(("MENU" . ,(file-append rofi "/bin/rofi -dmenu"))
         ("BROWSER" . ,(file-append ungoogled-chromium))
         ("TERMINAL" . ,(file-append alacritty))
         ("PATH" . (string-append
                    "${PATH}:"
                    "${HOME}/.local/bin:"
                    "${HOME}/.local/bin/statusbar:"
                    "${HOME}/.local/share/flatpak/exports/bin"))
         ("XDG_DATA_DIRS" . (string-append
                             "${XDG_DATA_DIRS}:"
                             "/var/lib/flatpak/exports/share:"
                             "${HOME}/.local/share/flatpak/exports/share"))
         ("WGETRC" . "${HOME}/.config/wget/wgetrc")
         ("LESSHISTFILE" . "-")
         ("SUDO_ASKPASS" . "${HOME}/.local/bin/menuaskpass")))))
    (feature-base-services)
    (feature-desktop-services)
    (feature-pipewire)
    (feature-backlight #:step 5)

    (feature-fonts
     #:font-monospace (font "Iosevka" #:size 14 #:weight 'regular)
     #:font-packages (list font-iosevka font-fira-mono))

    (feature-alacritty
     #:config-file (local-file "./config/alacritty.yml")
     #:default-terminal? #f
     #:backup-terminal? #t
     #:software-rendering? #f)
    (feature-vterm)
    (feature-zsh
     #:enable-zsh-autosuggestions? #t
     #:zshrc (list #~(string-append
                      "source " #$(local-file "./config/aliasrc"))))
    (feature-bash)
    (feature-direnv)
    (feature-git
     #:sign-commits? #t
     #:git-sign-key
     "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINd9BmbuU3HS6pbCzCe1IZGxaHHDJERXpQRZZiRkfL3a"
     #:git-send-email? #t)

    (feature-markdown)
    (feature-mpv)
    (feature-tmux)

    (feature-xdg
     #:xdg-user-directories-configuration
     (home-xdg-user-directories-configuration
      (download "$HOME/tmp")
      (videos "$HOME/archives/videos")
      (music "$HOME/archives/music")
      (pictures "$HOME/archives/img")
      (documents "$HOME/resources")))

    (feature-ydotool)

    (feature-base-packages
     #:home-packages
     (append (list rofi-power-menu
                   rofi-switch-browser-tabs
                   (@ (packages snapper) snapper))
      (strings->packages
       "hicolor-icon-theme" "adwaita-icon-theme" "papirus-icon-theme" ;; themes
       "pavucontrol" "alsa-utils"  ;; sound
       "youtube-dl"  ;; music
       "bluez"  ;; bluetooth
       "swappy" "grim" "slurp" "imv"  ;; image
       "ffmpeg"  ;; video
       "rsync" "zip" "libreoffice" "thunar"  ;; documents
       "ungoogled-chromium-wayland" "ublock-origin-chromium" "nyxt"  ;; browsers
       "libnotify" "wl-clipboard" "wev" "wlsunset" ;; wayland
       "recutils" "ripgrep" "curl"  ;; utils
       ;; other
       "nerd-dictation-wayland"
       "flatpak"
       "libxml2"
       "git-annex"
       "recutils"
       ))))
   %window-management-features
   %emacs-features
   %mail-features
   %ssh-feature))


;;; rde-config and helpers for generating home-environment and
;;; operating-system records.

(define %config
  (rde-config
   (features
    (append
     %user-features
     %main-features
     %host-features))))

(define %os
  (rde-config-operating-system %config))

(define %he
  (rde-config-home-environment %config))


;;; Live OS
(use-modules
 (gnu services))
(define live-config
  (rde-config
   (features
    (append
     %user-features %main-features
     (list
      (feature-file-systems
       #:file-systems live-file-systems)
      (feature-kernel
       #:kernel linux
       #:firmware (list linux-firmware))
      (feature-hidpi)
      (feature-custom-services
       #:feature-name-prefix 'live
       #:system-services
       (list (service gc-root-service-type (list %he)))))))))

(define live-os
  (rde-config-operating-system live-config))


;;; Dispatcher

(define (dispatcher)
  (let ((rde-target (getenv "RDE_TARGET")))
    (match rde-target
      ("home" %he)
      ("system" %os)
      ("live-system" live-os)
      ("live-install" live-usb)
      (_ %he)
      )))

;; (pretty-print-rde-config ixy-config)
;; (use-modules (gnu services)
;; 	     (gnu services base))
;; (display
;;  (filter (lambda (x)
;; 	   (eq? (service-kind x) console-font-service-type))
;; 	 (rde-config-system-services ixy-config)))

;; (use-modules (rde features))
;; ((@ (ice-9 pretty-print) pretty-print)
 ;; (map feature-name (rde-config-features live-install)))

(dispatcher)
