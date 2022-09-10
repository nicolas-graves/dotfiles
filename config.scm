;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>
(add-to-load-path (dirname (current-filename)))


;;; Channels
(use-modules
 (guix gexp)
 (ice-9 match)
 (ice-9 pretty-print))

(define* (dots-channel
          #:key
          (freeze? #f)
          (freeze-commits
           '((nonguix     . "674d04a5fbd8689ab5ff27271a656f711fc77c54")
             (rde         . "051e0f77aef5610d1e74745cf9e2303b034462c3")
             (guix        . "8f0d45ccac3f6cee69eba8de5e4ae5e5555f1a3d"))))
  "This function generates the .guix-channel file content, with optional
commit pinning."
  `(channel
    (version 0)
    (url "/home/graves/spheres/info/dots")
    (dependencies
     (channel
      (name nonguix)
      (url "https://gitlab.com/nonguix/nonguix")
      ,(if freeze? `(commit ,(cdr (assoc 'nonguix freeze-commits)))
           `(branch "master"))
      (introduction
       (make-channel-introduction
        "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
        (openpgp-fingerprint
         "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
     (channel
      (name rde)
      (url "https://git.sr.ht/~abcdw/rde")
      ,(if freeze? `(commit ,(cdr (assoc 'rde freeze-commits)))
           `(branch "master"))
      (introduction
       (make-channel-introduction
        "257cebd587b66e4d865b3537a9a88cccd7107c95"
        (openpgp-fingerprint
         "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
     (channel
      (name guix)
      (url "https://git.savannah.gnu.org/git/guix.git")
      ,(if freeze? `(commit ,(cdr (assoc 'guix freeze-commits)))
           `(branch "master"))
      (introduction
       (make-channel-introduction
        "9edb3f66fd807b096b48283debdcddccfea34bad"
        (openpgp-fingerprint
         "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))))

(define channels-file
  (plain-file "channels"
              "(list (channel (name 'dotfiles)
(url \"/home/graves/spheres/info/dots\")
(branch \"main\")))"))


;;; Hardware/host file systems

(use-modules
   (gnu system)
   (gnu system file-systems)
   (gnu system mapped-devices))

(define devices
  '(("Precision-3571" .
     ((efi         . /dev/nvme0n1p1)
      (swap        . /dev/nvme0n1p2)
      (uuid-mapped . 86106e76-c07f-441a-a515-06559c617065)))
    ("20AMS6GD00" .
     ((efi         . /dev/sda1)
      (swap        . /dev/sda2)
      (uuid-mapped . a9319ee9-f216-4cad-bfa5-99a24a576562)))
    ("2325K55" .
     ((efi         . /dev/sda1)
      (swap        . /dev/sda2)
      (uuid-mapped . 1e7cef7b-c4dc-42d9-802e-71a50a00c20b)))))

(define (lookup var)
  "This function looks up the value of the variable var on the current
device."
  (symbol->string
   (cdr (assoc var (cdr (assoc (gethostname) devices))))))

(define %mapped-device
  (mapped-device
   (source (uuid (string-append (lookup 'uuid-mapped))))
   (targets (list "enc"))
   (type luks-device-mapping)))

(define file-systems
  (append
   (map
    (match-lambda
      ((subvol . mount-point)
       (file-system
         (type "btrfs")
         ;;(device (file-system-label "enc"))
         (device "/dev/mapper/enc")
         (mount-point mount-point)
         (options
          (format
           #f "autodefrag,compress=zstd,ssd_spread,space_cache=v2,subvol=~a" subvol))
         (dependencies (list %mapped-device))
         )))
    '((root . "/")
      (store  . "/gnu/store")
      (home . "/home")
      ;; (snapshots . "/home/.snapshots")
      (data . "/data")
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

(define-public live-file-systems
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
    #:host-name (gethostname)
    #:timezone  "Europe/Paris")
   ;;; Allows to declare specific bootloader configuration,
   ;;; grub-efi-bootloader used by default
   (feature-bootloader)
   (feature-file-systems
    #:mapped-devices (list %mapped-device)
    #:swap-devices (list (swap-space (target (lookup 'swap))))
    #:file-systems  file-systems)
   (feature-kernel
    #:kernel linux
    #:initrd microcode-initrd
    #:initrd-modules
    (append (list "vmd") (@ (gnu system linux-initrd) %base-initrd-modules))
    #:kernel-arguments
    (append (list "quiet" "rootfstype=btrfs") %default-kernel-arguments)
    #:firmware (list linux-firmware))
   (feature-hidpi)))


;;; User Features
(use-modules
 (gnu system keyboard)
 (rde features gnupg)
 (rde features keyboard)
 (rde features password-utils))

(define-public %user-features
  (list
   (feature-user-info
    #:user-name "graves"
    #:full-name "Nicolas Graves"
    #:email "ngraves@ngraves.fr"
    #:user-initial-password-hash "gaAxdKLOplpY2"
    ;; (crypt "bob" "$6$abc")
    #:emacs-advanced-user? #t)
   (feature-gnupg
    #:gpg-ssh-agent? #t
    #:ssh-keys
    '(("4B8C7C409D8E286BAF9F1B075181FFE6E0AF7249")
      ("748668172FB0CE88407F006E6ABD649DDD3EF2DD")
      ("F204255D0F694AC6CEC585EFC21FFE27298B9D92"))
    #:gpg-primary-key "3F61A23D53B5B118"
    #:gpg-smart-card? #f
    #:pinentry-flavor 'qt)

   (feature-password-store
    #:remote-password-store-url "git@git.sr.ht:~ngraves/pass")

   (feature-keyboard
    ;; To get all available options, layouts and variants run:
    ;; cat `guix build xkeyboard-config`/share/X11/xkb/rules/evdev.lst
    #:keyboard-layout
    (keyboard-layout
     "fr,fr" "latin9,bepo"
     #:options '("caps:escape" "grp:shifts_toggle")))))


;;; USB install

;; Generic functions for packages
(use-modules (gnu packages))

(define* (pkgs #:rest lst)
  "This function converts list of string packages to actual packages."
  (map specification->package+output lst))

(define* (pkgs-vanilla #:rest lst)
  "Packages from guix channel."
  (define channel-guix
    (list (channel
           (name 'guix)
           (url "https://git.savannah.gnu.org/git/guix.git")
           ;; (commit "2b6af630d61dd5b16424be55088de2b079e9fbaf")
           )))

  (define inferior (inferior-for-channels channel-guix))
  (define (get-inferior-pkg pkg-name)
    (car (lookup-inferior-packages inferior pkg-name)))

  (map get-inferior-pkg lst))

(use-modules
 (rde features)
 (guix gexp)
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
        (pkgs "ripgrep" "vim" "git" "emacs-no-x" "zip" "unzip"
         "exfat-utils" "fuse-exfat" "ntfs-3g" "grub" "make" "glibc"
         "network-manager" "nss-certs" "curl"
         "fontconfig" "font-dejavu" "font-gnu-unifont" "font-terminus")
        %base-packages-disk-utilities
        %base-packages))
      (feature-base-services
       #:guix-substitute-urls
       (cons*
        "https://substitutes.nonguix.org"
        ;; (string-append "https://" (getenv "URI_service_substitutes"))
        (@ (guix store) %default-substitute-urls))
       #:guix-authorized-keys
       (cons*
        (local-file "./keys/nonguix.pub")
        (local-file "./keys/my-substitutes-key.pub")
        (@ (gnu services base) %default-authorized-guix-keys))
       #:base-services
       (let* ((path "/share/consolefonts/ter-132n")
              (font #~(string-append #$font-terminus #$path))
              (ttys '("tty1" "tty2" "tty3" "tty4" "tty5" "tty6")))
         (append
          (list
           (simple-service
            'channels-and-sources
            etc-service-type
            `(("channels.scm" ,channels-file)
              ("guix-sources" ,(local-file "../guix" #:recursive? #t))
              ("nonguix-sources" ,(local-file "../../../projects/src/nonguix"
                                              #:recursive? #t))
              ("rde-sources" ,(local-file "../rde" #:recursive? #t))
              ;;("dotfiles-sources" ,(local-file
              ;;                             #:recursive? #t))
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
(use-modules  ;; wm
 (home features wm)
 (rde features wm))

(define %window-management-features
  (list
   (ng-feature-sway
    #:xwayland? #t
    #:extra-config
    `((bindsym
       --to-code
       (($mod+Shift+s exec "grim -g \"$(slurp)\" - | swappy -f -")
        (Print exec "grim - | wl-copy -t image/png")
        ($mod+dollar exec makoctl dismiss --all)
        ($mod+exclam exec makoctl set-mode dnd)
        ($mod+Shift+exclam exec makoctl set-mode default)
        ($mod+p exec ~/.local/bin/menu_pass)
        ($mod+w exec chromium)
        ($mod+Shift+w exec chromium --incognito)
        ;; ($mod+Shift+o exec emacsclient -c -e "'(dired /home/graves)'")
        ($mod+m exec ~/.local/bin/playm)
        ($mod+Shift+m exec killall mpv)))

      (exec wlsunset -l 48.86 -L 2.35 -T 6500 -t 3000)
      (exec mako)

      (workspace_auto_back_and_forth yes)
      (focus_follows_mouse no)
      (smart_borders on)
      (title_align center)

      (output * bg
              ,(string-append
                (getenv "HOME")
                "/spheres/info/dots/home/share/fond_pre.jpg") fill)
      (output eDP-1 scale 1)

      (assign "[app_id=\"nyxt\"]" 3)
      (assign "[app_id=\"chromium-browser\"]" 3)

      (for_window
       "[app_id=\"^.*\"]"
       inhibit_idle fullscreen)
      (for_window
       "[title=\"^(?:Open|Save) (?:File|Folder|As).*\"]"
       floating enable, resize set width 70 ppt height 70 ppt)

      (client.focused "#EEEEEE" "#005577" "#770000" "#770000" "#770000")
      (client.unfocused "#BBBBBB" "#222222" "#444444")
      (seat * xcursor_theme Adwaita 24)

      ;; (bindswitch --reload --locked lid:on exec /run/setuid-programs/swaylock)

      ;; FIXME: Use absolute path, move to feature-network
      (exec nm-applet --indicator)

      ;; (bindsym $mod+Shift+o ,#~"[floating]" kill)
      (input type:touchpad
             ;; TODO: Move it to feature-sway or feature-mouse?
             ( ;; (natural_scroll enabled)
              (tap enabled)))))
   (feature-sway-run-on-tty
    #:sway-tty-number 1)
   (ng-feature-sway-screenshot)
   (feature-waybar
    #:waybar-modules
    (list
     (waybar-sway-workspaces)
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
    (local-file "../rde/rde/features/wm/waybar/base16-default-dark.css"))
   (feature-swayidle)
   (feature-swaylock
    #:swaylock (@ (gnu packages wm) swaylock-effects)
    ;; The blur on lock screen is not privacy-friendly.
    #:extra-config
    '(;; (screenshots)
      ;; (effect-blur . 7x5)
      (clock)
      (image . /home/graves/spheres/info/dots/home/share/fond_lock_pre.jpg)))))


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
            (string-drop-right (car file) (string-length ".gpg"))
            (+ 1 (string-length passdir))))
         (find-files passdir "@[-a-z\\.]+\\.[a-z]{2,3}\\.gpg$"))))))

(define (id->type id)
  (cond
    ((string=? id "neleves") 'enpc)
    ((string=? id "ngmx") 'gmx-fr)
    ((string=? id "ngmail") 'gmail)
    ((string=? id "epour-un-reveil-ecologique") 'ovh-pro2)
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
   (pass-cmd (string-append "pass show " user " | head -1"))))))

(define %msmtp-provider-settings
  (acons 'enpc '((host . "boyer2.enpc.fr")
                 (port . 465)
                 (tls_starttls . off))
          %default-msmtp-provider-settings))

(define my-msmtp-provider-settings
  (acons 'ovh-pro2 '((host . "pro2.mail.ovh.net")
                    (port . 587))
      %msmtp-provider-settings))

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

;; See [[id:76111cb0-50b1-48ca-afba-053c46ab2f98][imap-utf7]].
(define ovh-pro-fr-folder-mapping
  '(("inbox"   . "INBOX")
    ("sent"    . "&AMk-l&AOk-ments envoy&AOk-s") ;"Éléments envoyés"
    ("drafts"  . "Brouillons")
    ("archive" . "Notes")
    ("trash"   . "&AMk-l&AOk-ments supprim&AOk-s") ;"Éléments supprimés"
    ("spam"    . "Courrier ind&AOk-sirable"))) ;"Courrier indésirable"

(define (ovh-pro-isync-settings n)
  (generate-isync-serializer
    (string-append "pro" n ".mail.ovh.net")
    ovh-pro-fr-folder-mapping
    #:auth-mechs 'LOGIN
    #:subfolders 'Legacy))

(define enpc-isync-settings
  (generate-isync-serializer "messagerie.enpc.fr"
    (@@ (rde features mail) gandi-folder-mapping)
    #:cipher-string 'DEFAULT@SECLEVEL=1
    #:pipeline-depth 1))

(define %%isync-serializers
  (acons 'enpc enpc-isync-settings
         %default-isync-serializers))

(define %isync-serializers
  (acons 'ovh-pro2 (ovh-pro-isync-settings "2")
          %%isync-serializers))

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
    #:msmtp-provider-settings my-msmtp-provider-settings)
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

(use-modules ;;ssh
  (gnu packages ssh)
  (rde features ssh)
  (home services ssh-utils))

(define %ssh-feature
  (list
   (feature-ssh
    #:ssh-configuration
    (home-ssh-configuration
     (package openssh-sans-x)
     (toplevel-options
      '((match . "host * exec \"gpg-connect-agent UPDATESTARTUPTTY /bye\"")))
     (user-known-hosts-file
      ;; This file is private, hidden, and writable on purpose.
      '("~/spheres/info/dots/home/config/ssh/known_hosts"
        "~/.ssh/my_known_hosts"))
     (default-host "*")
     (default-options
       '((address-family . "inet")))
     (extra-config
      `(,(car (ssh-config "my_git"))
        ,(car (ssh-config "my_server"))
        ,(car (ssh-config "my_dev"))
        ,(car (ssh-config "inari"))
        ,(car (ssh-config "pre_site"))
        ,(car (ssh-config "pre_bitwarden"))))
     ))))

(define ssh-files
  (list
   `(".ssh/id_rsa.pub" ,(local-file "keys/id_rsa.pub"))
   `(".ssh/id_ed25519.pub" ,(local-file "keys/id_ed25519.pub"))
   `(".ssh/id_rsa_git.pub" ,(local-file "keys/id_rsa_git.pub"))
   `(".ssh/my_known_hosts"
     ,(plain-file "my_known_hosts"
                  (string-append
                   (car (cdr (ssh-config "my_git")))
                   (car (cdr (ssh-config "my_server")))
                   (car (cdr (ssh-config "my_dev")))
                   (car (cdr (ssh-config "pre_site")))
                   (car (cdr (ssh-config "pre_bitwarden")))
                   (car (cdr (ssh-config "inari"))))))
   ))


;;; Emacs
(use-modules
 (home features emacs)
 (rde features emacs)
 (rde features emacs-xyz)
 (rde packages emacs)
 (rde packages emacs-xyz))

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
    #:emacs
    (if (string=? (or (getenv "BUILD_SUBMITTER") "") "git.sr.ht")
        (@ (gnu packages emacs) emacs-next-pgtk)
        emacs-next-pgtk-latest)
    #:extra-init-el
    `(;; using external programs sometimes requires having this variable set
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

      (require 'git-annex)

      ;; html email and elfeed
      (setq shr-current-font "Iosevka")

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
     (pkgs "emacs-hl-todo"
           "emacs-consult-dir"
           "emacs-dirvish"
           "emacs-restart-emacs"
           "emacs-git-annex"
           "emacs-magit-annex"
           "emacs-app-launcher"
           "emacs-mini-frame"
           "emacs-biblio@0.2-0.72ddab0"
           "emacs-consult-org-roam"
           "emacs-git-email"
           "emacs-emojify"
           "emacs-ol-notmuch"
           "python-lsp-server"
           "emacs-org-pomodoro"))
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
    #:olivetti-body-width 100)

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

   (feature-emacs-my-org-agenda
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
    #:nerd-commenter? #t
    )
   (feature-emacs-saving)
   (feature-emacs-elfeed
    #:elfeed-org-files '("~/resources/feeds.org")
    #:capture-in-browser? #t)
   (feature-emacs-org-protocol)
   (feature-emacs-dired-hacks
    #:evil? #t)
   (feature-emacs-guix-development
    #:guix-load-path "/home/graves/spheres/info/guix"
    #:other-guile-load-paths (list "/home/graves/spheres/info/rde")
    #:snippets-path "~/.config/guix/snippets/*.eld")
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
   ;; Unfonctionnal, as if the package emacs-eval-in repl wasn't existing.
   (feature-emacs-eval-in-repl
    #:load-language-list
    (list "emacs-lisp" "python" "shell" "scheme")
    #:repl-placement "right"
    #:rely-on-geiser? #t)
   (feature-emacs-origami-el)
   (feature-emacs-python)
   (feature-emacs-web-mode
    #:rainbow-mode? #t)
   (feature-emacs-yaml-mode)
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
 (home features xdisorg)
 (rde features markup)
 (rde features video)
 (rde features tmux)
 (rde features xdg)
 (gnu home services)
 (gnu home services xdg)
 (gnu home services shells)
 (gnu packages xdisorg)
 (packages xdisorg)
 (gnu packages chromium)
 (gnu packages terminals))

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
        `("guix/channels.scm" ,channels-file)
        `("guix/snippets"
          ,(local-file "home/config/guix/snippets" #:recursive? #t))
        `("shell/aliasrc" ,(local-file "home/config/aliasrc"))
        `("wget/wgetrc" ,(plain-file "wgetrc" "hsts-file=~/.cache/wget-hsts\n"))))
      (service
       home-files-service-type
       `(
        ;; ,ssh-files
        ;; `(".xkb/symbols/programmer_beop"
        ;; ,(local-file "home/config/xkb/symbols/programmer_beop"))
        (".local/bin" ,(local-file "home/scripts" #:recursive? #t))
        ))
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
                    "${HOME}/.local/bin/cron:"
                    "${HOME}/.local/share/flatpak/exports/bin"))
         ("XDG_DATA_DIRS" . (string-append
                             "${XDG_DATA_DIRS}:"
                             "/var/lib/flatpak/exports/share:"
                             "${HOME}/.local/share/flatpak/exports/share"))
         ("WGETRC" . "${HOME}/.config/wget/wgetrc")
         ("LESSHISTFILE" . "-")
         ("SUDO_ASKPASS" . "${HOME}/.local/bin/menuaskpass")))
      ;; FIXME bash aliases work in alacritty but not in eshell.
      ;; (simple-service
      ;;  'bash-aliases
      ;;  home-bash-service-type
      ;;  (home-bash-extension
      ;;   (bashrc
      ;;    '("source /home/graves/.config/shell/aliasrc"))))
      ;; (simple-service
      ;;  'zsh-aliases
      ;;  home-zsh-service-type
      ;;  (home-zsh-extension
      ;;   (zshrc
      ;;    '("source /home/graves/.config/shell/aliasrc"))))
      ))

    (feature-base-services)
    (feature-desktop-services)
    (feature-pipewire)
    (feature-backlight #:step 5)

    (feature-fonts
     #:font-monospace (font "Iosevka" #:size 14 #:weight 'regular)
     #:font-packages (list font-iosevka font-fira-mono))

    (feature-alacritty
     #:config-file (local-file "./home/config/alacritty.yml")
     #:default-terminal? #f
     #:backup-terminal? #t
     #:software-rendering? #f)
    (feature-vterm)
    (feature-zsh
     #:enable-zsh-autosuggestions? #t)
    (feature-bash)
    (feature-direnv)
    (feature-git
     #:sign-commits? #t
     #:git-gpg-sign-key "3F61A23D53B5B118"
     #:git-send-email? #t)

    (feature-rofi)
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
      (documents "$HOME/resources")
      (publicshare "$HOME")
      (templates "$HOME")
      (desktop "$HOME")))

    (feature-base-packages
     #:home-packages
     (append (list rofi-power-menu)
      (pkgs
       ;; themes
       "hicolor-icon-theme"
       "adwaita-icon-theme"
       "papirus-icon-theme"
       ;; sound
       "pavucontrol"
       "alsa-utils"
       "youtube-dl" ;; music
       "bluez" ;; bluetooth
       ;; image
       "swappy"
       "grim"
       "slurp"
       "imv"
       "ffmpeg" ;; video
       ;; documents
       "rsync"
       "libreoffice"
       "thunar"
       ;; browsers
       "ungoogled-chromium-wayland"
       "ublock-origin-chromium"
       "nyxt"
       ;; other
       "flatpak"
       "libnotify"
       "libxml2"
       "wl-clipboard"
       "wev"
       "recutils"
       "ripgrep"
       "curl"
       "snapper"
       "wlsunset"
       "git-annex"
       ))))
   %window-management-features
   %emacs-features
   %mail-features
   %ssh-feature))


;;; rde-config and helpers for generating home-environment and
;;; operating-system records.

(define-public %config
  (rde-config
   (features
    (append
     %user-features
     %main-features
     %host-features))))

;; TODISCUSS: Make rde-config-os/he to be a feature instead of getter?
(define-public %os
  (rde-config-operating-system %config))

(define %he
  (rde-config-home-environment %config))


;;; Live OS
(use-modules
 (gnu services))
(define-public live-config
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

(define-public live-os
  (rde-config-operating-system live-config))


;;; Dispatcher

(define (dispatcher)
  (let ((rde-target (getenv "RDE_TARGET")))
    (match rde-target
      ("home" %he)
      ("system" %os)
      ("live-system" live-os)
      ("live-install" live-usb)
      ("channel"
        (with-output-to-file ".guix-channel"
           (lambda () (pretty-print (dots-channel)))))
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
