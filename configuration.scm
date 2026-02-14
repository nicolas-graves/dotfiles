;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2021-2025 Nicolas Graves <ngraves@ngraves.fr>

;; I embraced system crafting for long-term resilience and efficiency reasons.
;; This repository is a resource for cherry-picking code snippets and holds all
;; my configs, made clean and compact by RDE and GNU Guix.

;; To develop Guix/RDE rapidly, I use local repositories and tooling.
;; This file uses a dispatch to work with several commands: guix rde/home/system
;; See used channels at ./channels.scm
;; Tip: to sign commits when broken: `git --no-gpg-sign'

(use-modules (ice-9 ftw)
             (ice-9 popen)
             (ice-9 rdelim)
             (srfi srfi-1)
             (srfi srfi-2)
             (srfi srfi-26)

             (guix build utils)
             (guix derivations)
             (guix download)
             (guix gexp)
             (guix git-download)
             (guix monads)
             (guix packages)
             (guix store)

             ((gnu services) #:select (simple-service))

             ((guix packages) #:select (origin base32 package))
             (guix records)
             ((guix ui) #:select (with-error-handling))
             ((guix utils) #:select (readlink*))
             ((gnu services) #:select (simple-service etc-service-type service))
             (guix build-system channel)
             (guix build-system font)
             (gnu packages fonts))

;; Modules depending on more than guix.
(use-modules (rde features)
             (rde packages)
             (rde containers)
             (rde home services emacs))

(eval-when (eval load compile)
  (begin
    (define (name->feature-module name)
      `(rde features ,(string->symbol name)))
    (define %feature-modules
      (map (compose name->feature-module (cut string-drop-right <> 4))
           (scandir (find directory-exists?
                          (map (cut string-append <> "/rde/features")
                               %load-path))
                    (cut string-suffix? ".scm" <>))))

    ((@@ (gnu) %try-use-modules) %feature-modules #f (const #t))))

(define-syntax-rule (or@ module name)
  "Like (@ MODULE NAME), but returns #f instead of error if it fails."
  (false-if-exception (module-ref (resolve-module 'module) 'name)))

(define cwd (dirname (current-filename)))

(define (sanitize-home-string str homedir)
  (if (string-prefix? "~" str)
      (string-append homedir (string-drop str 1))
      str))

(define (find-home str)
  (sanitize-home-string str (getenv "HOME")))

(use-modules
 ;; Modules for make.
 (srfi srfi-1) (srfi srfi-9 gnu) (srfi srfi-71) (srfi srfi-26) (git)
 (ice-9 match) (ice-9 textual-ports) (ice-9 popen)
 (guix gexp) (guix build utils) (guix channels) (guix records)
 (gnu home) (gnu system)
 (gnu system image) ((gnu image) #:hide (partition))
 ;; ((rde features) #:select (rde-config-home-environment
                           ;; rde-config-operating-system))
 ((guix profiles) #:select (manifest-entries
                            manifest-entry-properties
                            %profile-directory
                            profile-manifest
                            generation-number
                            generation-file-name))
 ((guix store) #:select (with-store run-with-store
                          %store-monad mapm/accumulate-builds))

 ;; Modules for machine helpers.
 (guix records)
 (srfi srfi-1) (ice-9 popen) (ice-9 rdelim) (ice-9 match)
 (gnu system) (gnu system file-systems) (gnu system mapped-devices)
 (gnu system uuid)
 (gnu packages linux)

 ;; Modules for config.
 (ice-9 popen) (ice-9 rdelim)
 (srfi srfi-1) (ice-9 match)
 (gnu system)
 ((guix build utils) #:select (find-files))
 ((gnu packages) #:select (specification->package))
 ((gnu services) #:select (simple-service etc-service-type service))
 ((guix download) #:select (url-fetch url-fetch/zipbomb))
 ((guix packages) #:select (origin base32 package))
 ((guix ui) #:select (with-error-handling))
 ((guix utils) #:select (readlink*))
 (guix build-system channel)
 (guix build-system font) (gnu packages fonts)
 (guix gexp) (guix packages) (guix git-download) (guix git) (guix monads)
 (guix scripts system) (guix scripts system reconfigure) (gnu bootloader)

 ;; Modules for live config

 ;; Other modules.
 (gnu services)
 (gnu system file-systems)
 (gnu packages emacs-xyz)
 (gnu home services)
 ;; (gnu home services guix)
 ;; (gnu home services ssh)
 (guix derivations))

(use-modules
 (rde features)
 (rde features system)
 ((rde packages) #:select (strings->packages))
 (rde home services emacs)
 (contrib features emacs-xyz)
 (contrib features age))

(define config-file
  (string-append (dirname (current-filename)) "/configuration.scm"))

(define btrbk-conf
  (string-append (dirname (current-filename)) "/files/btrbk.conf"))


(use-modules (gnu services base))

(define nonguix-service
  (delay
    (simple-service
     'nonguix
     guix-service-type
     (guix-extension
      (substitute-urls (list "https://substitutes.nonguix.org"))
      (authorized-keys
       (list
        (origin
          (method url-fetch)
          (uri "https://substitutes.nonguix.org/signing-key.pub")
          (sha256
           (base32
            "0j66nq1bxvbxf5n8q2py14sjbkn57my0mjwq7k1qm9ddghca7177")))))))))

(define guix-science-service
  (delay
    (simple-service
     'guix-science
     guix-service-type
     (guix-extension
      (substitute-urls (list "https://guix.bordeaux.inria.fr"))
      (authorized-keys
       (list
        (origin
          (method url-fetch)
          (uri "https://guix.bordeaux.inria.fr/signing-key.pub")
          (sha256
           (base32
            "056cv0vlqyacyhbmwr5651fzg1icyxbw61nkap7sd4j2x8qj7ila")))))))))


;; Machine record and %current-machine
(define-record-type* <machine> machine make-machine
  machine?
  this-machine
  (name machine-name)                                    ; string
  (efi machine-efi)                                      ; file-system
  (encrypted-uuid-mapped machine-encrypted-uuid-mapped   ; maybe-uuid
                         (default #f))
  (btrfs-layout machine-btrfs-layout                     ; alist
                (default (root-impermanence-btrfs-layout)) (delayed))
  (architecture machine-architecture                     ; string
                (default "x86_64-linux"))
  (firmware machine-firmware                             ; list of packages
            (delayed)
            (default '()))
  (nvidia? machine-nvidia?                               ; boolean
           (default #f))
  (kernel-build-options machine-kernel-build-options     ; list of options
                        (default '()))
  ;; SSH key identifying the ssh daemon, found in /etc/ssh/ssh_host_ed25519_key.pub
  (ssh-host-key machine-ssh-host-key                     ; string
                (default #f))
  ;; SSH key used to connect between machines (as in ssh -i).
  (ssh-privkey-location machine-ssh-privkey-location     ; string
                        (default #f))
  (ssh-pubkey machine-ssh-pubkey                         ; string
              (default #f))
  ;; key to authentify archives, found in /etc/guix/signing-key.pub
  (guix-pubkey machine-guix-pubkey                         ; string
               (default #f)))

(define %machines
  (list
   (machine (name "precision")
            (efi "/dev/nvme0n1p1")
            (encrypted-uuid-mapped "92f9af3d-d860-4497-91ea-9e46a1dacf7a")
            (btrfs-layout (append '(;;(data . "/data")
                                    (btrbk_snapshots . "/btrbk_snapshots"))
                                  root-impermanence-btrfs-layout
                                  home-impermanence-para-btrfs-layout))
            (firmware (or (and=> (or@ (nongnu packages linux) linux-firmware)
                                 list)
                          '()))
            (nvidia? #t)
            (ssh-host-key "\
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKFEHSLyMo2hdIMmeRhaT1uObwahRqaQqHnAe0/bqLXn")
            (ssh-privkey-location "/home/graves/.local/share/ssh/id_ed25519")
            (ssh-pubkey "\
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJENtxo6OSdamVVqPlvwBrI5QLe4Wj4244cf51ubp/Uh")
            (guix-pubkey "\
F3A63087C01CA919484F7BB51FE81E20929D491AA1346FE0FC410CB1216EDB0A"))
   (machine (name "20xwcto1ww")
            (efi "/dev/nvme0n1p1")
            (encrypted-uuid-mapped "9dbcac0f-e5bd-45fc-a7f2-5841c5ea71b9")
            (btrfs-layout (append '(;;(data . "/data")
                                    (btrbk_snapshots . "/btrbk_snapshots"))
                                  root-impermanence-btrfs-layout
                                  home-impermanence-para-btrfs-layout))
            (ssh-host-key "\
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAID3dDHB5z2hr6ngtjj7TvXzbovUdhGzAODifATQdSJN5")
            (ssh-privkey-location "/home/graves/.local/share/ssh/id_ed25519")
            (ssh-pubkey "\
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJENtxo6OSdamVVqPlvwBrI5QLe4Wj4244cf51ubp/Uh")
            (guix-pubkey "\
892E3653363EEF353DDC583A434D3614502D450A4655D1B14D5242AAE6D90B46")
            (firmware (or (and=> (or@ (nongnu packages linux) iwlwifi-firmware)
                                 list)
                          '())))
   (machine (name "2325k55")
            (efi "/dev/sda1")
            (encrypted-uuid-mapped "824f71bd-8709-4b8e-8fd6-deee7ad1e4f0")
            (btrfs-layout (cons* '(home . "/home") root-impermanence-btrfs-layout))
            (firmware (or (and=> (or@ (nongnu packages linux) iwlwifi-firmware)
                                 list)
                          '()))
            (ssh-host-key "\
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM+hUmwvYmS8BC2HupASOnn88gLkeeZli7b+ji6Wz/M4")
            (ssh-privkey-location "/home/graves/.ssh/id_ed25519")
            (ssh-pubkey "\
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPpGldYnfml+ffHz8EuYMUoHXivuhTKzkdUYcIP/f1Bk"))
   ;; Might use r8169 module but it works fine without, use linux-libre then.
;;    (machine (name "optiplex")
;;             (efi "/dev/sda1")
;;             (encrypted-uuid-mapped "ad1b7435-9957-424d-b9ac-9a9eac040e72")
;;             (btrfs-layout (cons* '(home . "/home") root-impermanence-btrfs-layout))
;;             (ssh-host-key "\
;; ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICc0KTnwphWQ7jm/C9C48o8HAU2Ee4fViAoUvj6w80x1")
;;             (ssh-privkey-location "/home/graves/.ssh/id_ed25519")
;;             (ssh-pubkey "\
;; ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEvBo8x2khzm1oXLKWuxA3GlL29dfIuzHSOedHxoYMSl")
;;             (guix-pubkey "\
;; 1BEC0CE366F2325E65FEE419BC43DAACDDF0F334FF8E7B018687557C0B60BB16"))
   ))

(define %current-machine
  (let* ((raw-name (call-with-input-file
                       "/sys/devices/virtual/dmi/id/product_name"
                     read-line))
         (name (string-downcase (car (string-split raw-name #\ )))))
    (find (lambda (in) (equal? name (machine-name in)))
          %machines)))


;;; Live systems.
(define my-installation-os
  (delay
    (begin
      (use-modules (rde features)
                   (rde features base)
                   (rde features keyboard)
                   (rde features system)
                   (rde features linux)
                   (gnu services networking)
                   (gnu system install)
                   (nongnu packages linux)
                   (rde packages)
                   (gnu services)
                   (guix-stack channel-submodules))
      (rde-config-operating-system
       (rde-config
        (initial-os installation-os)
        (features
         (list
          (feature-keyboard
           #:keyboard-layout
           (keyboard-layout "fr" "," #:options '("caps:escape")))
          (feature-hidpi)
          (feature-kernel
           #:kernel linux
           #:firmware (or (and=> (or@ (nongnu packages linux) linux-firmware)
                                 list)
                          '()))
          (feature-base-packages
           #:system-packages
           (strings->packages "git" "zip" "unzip" "curl"
                              "exfat-utils" "fuse-exfat" "ntfs-3g"))
          (feature-custom-services
           #:feature-name-prefix 'live
           #:system-services
           (list
            (simple-service
             'channels-and-sources
             etc-service-type
             `(("guix/channels.scm"
                ,(scheme-file
                  "channels.scm"
                  `(list
                    ,@(map channel-instance->sexp*
                           (submodules-dir->channel-instances
                            "channels"
                            #:type '(branch . (or "origin/master" "origin/main")))))))
               ;; ("guix-sources" ,(local-file "/home/graves/spheres/info/guix" #:recursive? #t))
               ;; ("nonguix-sources" ,(local-file "/home/graves/spheres/info/nonguix" #:recursive? #t))
               ;; ("rde-sources" ,(local-file "/home/graves/spheres/info/rde" #:recursive? #t))
               ))
            (service wpa-supplicant-service-type)
            (service network-manager-service-type)
            (service (@@ (gnu system install) cow-store-service-type) 'mooh!)))
          (feature-shepherd)
          (feature-base-services)
          (feature-custom-services
           #:feature-name-prefix 'more-substitutes
           #:system-services (list (force nonguix-service)
                                   (force guix-science-service))))))))))

(when (member (machine-name %current-machine) (list "precision" "20xwcto1ww"))
  (use-modules (gnu packages emacs-xyz)
               (rde packages emacs-xyz)
               (contrib features age)
               (contrib features emacs-xyz)
               (contrib features machine-learning)
               (contrib features task-runners)
               (contrib packages machine-learning)))
(use-modules (srfi srfi-2))


;; Privacy without GNUPG: currently using age with ssh and git commit signing. ;; TODO more details later.
;; Tip: sign outside git with ssh: `ssh-keygen -Y sign -n "file" -f /.ssh/id_ed25519_sk < "${file_to_sign}" > "${file_to_sign}.asc"'

(define %user-features
  (list
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
(define background
  (origin
    (method url-fetch)
    (uri "https://pour-un-reveil-ecologique.org/media/images/fond_pre.original.jpg")
    (file-name "fond_pre.jpg")
    (sha256 (base32 "03rn4fw9j31s7hl635q872hzxj4bj5m9hkjd4iqzl8z4lk0n9iiy"))))

(define %wm-features
  (list
   (feature-hidpi)
   (feature-sway
    #:xwayland? #f
    #:extra-config
    `((bindsym
       --to-code
       (;; ($mod+w exec chromium --remote-debugging-port=9222)
        ;; ($mod+Shift+w exec chromium --incognito --remote-debugging-port=9222)
        ($mod+Shift+m exec killall mpv)))

      ,@(append-map
         (lambda (previous-key key number)
           `((unbindsym ,(format #f "$mod+~a" previous-key))
             (unbindsym ,(format #f "$mod+Shift+~a" previous-key))
             (bindsym ,(format #f "$mod+~a" key)
                      workspace number ,number)
             (bindsym ,(format #f "$mod+Shift+~a" key)
                      move container to workspace number ,number)))
         (append (iota 9 1) '(0))
         '(ampersand
           eacute
           quotedbl
           apostrophe
           parenleft
           minus
           egrave
           underscore
           ccedilla
           agrave)
         (iota 10 1))

      (exec wlsunset -l 48.86 -L 2.35 -T 6500 -t 3000)

      (workspace_auto_back_and_forth yes)
      (focus_follows_mouse no)
      (smart_borders on)
      (title_align center)

      (output * bg ,background fill)

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
  (let ((passdir (string-append cwd "/files/pass")))
    (cons*
     "ngraves@ngraves.fr"  ; ensuring primary_email
     (delete "ngraves@ngraves.fr"
             (map (lambda file
                    (string-drop
                     (string-drop-right (car file) (string-length ".age"))
                     (+ 1 (string-length passdir))))
                  (find-files passdir "@[-a-z\\.]+\\.[a-z]{2,3}\\.age$"))))))

(define (id->type id)
  (cond
   ((string=? id "ngmx") 'gmx-fr)
   ((string=? id "ngmail") 'gmail)
   ((string=? id "nngraves") 'ovh)
   (else #f)))

(define (user->id user)
  (string-append
   (string-take user 1)
   (car (string-split (car (cdr (string-split user #\@))) #\.))))

(define* (single-mail-acc user)
  "Make a simple mail-account with ovh type by default."
  (and-let* ((id_ (user->id user))
             (type (id->type id_)))
    (mail-account
     (id (string->symbol id_))
     (fqda user)
     (type type)
     (pass-cmd (string-append "passage show " user " | head -1")))))

(define* (mail-lst id fqda urls)
  "Make a simple mailing-list."
  (mailing-list
   (id   id)
   (fqda fqda)
   (config (l2md-repo
            (name (symbol->string id))
            (urls urls)))))

(define %mail-features
  (delay
    (list
     (feature-mail-settings
      #:mail-accounts
      (filter-map single-mail-acc %mail-list)
      #:mail-directory-fn (const (string-append (getenv "XDG_STATE_HOME") "/mail"))
      #:mailing-lists (list (mail-lst 'guix-devel "guix-devel@gnu.org"
                                      '("https://yhetil.org/guix-devel/0"))
                            (mail-lst 'guix-bugs "guix-bugs@gnu.org"
                                      '("https://yhetil.org/guix-bugs/0"))
                            (mail-lst 'guix-patches "guix-patches@gnu.org"
                                      '("https://yhetil.org/guix-patches/1"))))
     (feature-msmtp)
     (feature-isync
      #:mail-account-ids
      (append-map
       (lambda (x) (list (string->symbol (user->id x)))) %mail-list)
      #:isync-verbose #t)
     (feature-notmuch)
     (feature-l2md)
     (feature-emacs-piem
      #:piem-inboxes `(("rde-devel"
                        :url "https://lists.sr.ht/~abcdw/rde-devel"
                        :address "~abcdw/rde-devel@lists.sr.ht"
                        :coderepo (,(string-append (getcwd) "/channels/rde/")
                                   "~/projects/src/emacs-arei/"
                                   "~/projects/src/guile-ares-rs/"))
                       ("guix-patches"
                        :url "https://yhetil.org/guix-patches/"
                        :address "guix-patches@gnu.org"
                        :coderepo ,(string-append (getcwd) "/channels/rde/")))))))


;;; SSH
(define private-key
  (find-home "~/.local/share/ssh/id_encrypt"))

;; Maybe there a better way to send the pipe into a guix shell with all three
;; packages given with (specifications->manifest)
(define* (package-get-file store package
                           #:optional file
                           #:key (output "out") (system (%current-system)))
  "Return the absolute file name of FILE within the OUTPUT directory of
PACKAGE for SYSTEM.  When FILE is omitted, return the name of the OUTPUT
directory of PACKAGE for SYSTEM.

This procedure builds upon package-file and package-output, but _does_ build
PACKAGE when it's not available in the store.  Note that this procedure calls
`package-derivation', which is costly."
  (let* ((drv (package-derivation store package system))
         (out (derivation->output-path drv output))
         (result (and file (string-append out file))))
    (unless (directory-exists? out)
      (build-derivations store (list drv)))
    (or (and (file-exists? result) result) out)))

(define (ssh-config id)
  (let* ((age
          (with-store store
            (package-get-file
             store (@ (gnu packages golang-crypto) age) "/bin/age")))
         (passage
          (with-store store
            (package-get-file
             store (@ (gnu packages password-utils) pass-age) "/bin/passage")))
         (env
          (with-store store
            (package-get-file
             store (@ (gnu packages base) coreutils-minimal) "/bin/env")))
         (port
          (open-input-pipe
           (string-append
            env " -S"
            " PASSAGE_AGE=" age
            " PASSAGE_DIR=" (string-append cwd "/files/pass")
            " PASSAGE_IDENTITIES_FILE=" private-key
            " PASSAGE_RECIPIENTS_FILE=" private-key ".pub "
            passage " show ssh/ssh_" id " 2>/dev/null")))
         (key (read-line port))
         (alist (recutils->alist port)))
    (close-pipe port)
    (ssh-host
     (host id)
     (options
      `((hostname . ,(assoc-ref alist "URI"))
        (identity-file . ,(string-append "~/.local/share/ssh/" key))
        (port . ,(and=> (assoc-ref alist "Port") string->number))
        (user . ,(assoc-ref alist "Username")))))))

(define %ssh-feature
  (delay
    (feature-ssh
     #:ssh-agent? #t
     #:ssh-configuration
     (home-ssh-configuration
      (package (@ (gnu packages ssh) openssh-sans-x))
      (user-known-hosts-file
       '("/home/graves/.local/share/ssh/known_hosts"))
      (default-host "*")
      (default-options
        '((address-family . "inet"))))
     #:ssh-add-keys '("/home/graves/.local/share/ssh/id_sign"))))


;;; Emacs
;; TODO Find a way to clarify current organization with much less text, more code.
;; PARA method (https://fortelabs.com/blog/para/) with directories spheres/resources/projects/archives.
;;             with resources managed with: see ./hooks/git-biblio-prepare-commit-msg
(use-modules (rde build))

(define %org-agenda-custom-commands
  ''(("ca" "Custom: Agenda TODO [#A] items"
      ((org-ql-block '(and (todo "TODO")
                           (priority "A"))
                     ((org-ql-block-header "TODO : High-priority")))))
     ("ct" "Custom: Agenda TODO items"
      ((org-ql-block '(todo "TODO")
                     ((org-ql-block-header "TODO : All items")))))))

(define (slurp-sexps file)
  "Read FILE and return its contents as a list of S-expressions."
  (call-with-input-file file
    (lambda (port)
      (let loop ((acc '()))
        (let ((form (read port)))
          (if (eof-object? form)
              (reverse acc)
              (loop (cons form acc))))))))

(define %extra-init-el
  `(;; Use RDE style.
    (let ((rde-class (dir-locals-read-from-dir
                      ,(string-append cwd "/channels/rde"))))
      (dir-locals-set-directory-class ,cwd rde-class)
      (dir-locals-set-class-variables
       (intern ,cwd)
       '((nil
          . ((eval
              . (progn
                 (unless (boundp 'geiser-guile-load-path)
                   (defvar geiser-guile-load-path '()))
                 (make-local-variable 'geiser-guile-load-path)
                 (mapc
                  (lambda (channel)
                    (add-to-list
                     'geiser-guile-load-path
                     (concat ,(string-append cwd "/channels/") channel)))
                  (list "guix" "nonguix" "odf-dsfr" "rde"))))))))
      (dir-locals-set-directory-class ,cwd (intern ,cwd)))

    (defun format-xml ()
      "Format XML files using libxml2."
      (interactive)
      (shell-command-on-region
       1 (point-max)
       ,(file-append (@(gnu packages xml) libxml2) "/bin/xmllint --format -")
       (current-buffer) t))
    ;; clocking
    (setq org-clock-persist 'history)
    (org-clock-persistence-insinuate)
    (setq
     org-clock-persist-file
     (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
             "/emacs/org-clock-save.el"))
    ;; clocking in the task when setting a timer on a task
    (add-hook 'org-timer-set-hook 'org-clock-in)

    ;; html email and elfeed
    (setq shr-current-font "Iosevka")

    ;; help in learning keybindings
    (global-set-key (kbd "s-?") (lambda () (interactive) (embark-bindings t)))

    ;; fast smartparens toggle
    (global-set-key (kbd "s-)") 'smartparens-strict-mode)

    ;; don't save command-history, can be very big and not that useful
    (with-eval-after-load 'savehist
      (add-to-list 'savehist-ignored-variables 'command-history))

    ;; bibliography
    (setopt citar-library-file-extensions '("pdf.lz" "pdf" "docx.lz"))
    (setopt citadel-resources-directory "~/resources")
    (setopt citadel-dois-file "dois.txt")
    (setopt citadel-gen-bib-file "gen.bib")

    ;; Functions we'd rather define in their own file.
    ,@(slurp-sexps (string-append cwd "/configuration.el"))))

(define %additional-elisp-packages
  (cons*
   (let ((commit "12c5845efc95950b73498fcec3b1443c66742759")
         (revision "1"))
     (package
       (name "emacs-citadel")
       (version (git-version "0.0.0" revision commit))
       (home-page "https://codeberg.org/nicolas-graves/citadel")
       (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url home-page)
                (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0byvy34j3zjcdivnl2qbg3mp9ig5zw1hp7fbmp5rl51vampxk98k"))))
       (build-system (@ (guix build-system emacs) emacs-build-system))
       (propagated-inputs (list emacs-parsebib))
       (synopsis "Zotero in Emacs without Zotero")
       (description synopsis)
       (license (@ (guix licenses) gpl3+))))
   ;; (let ((commit "24164db7c323488fabd72d5f725254721f309573")
   ;;       (revision "0"))
   ;;   (package
   ;;     (inherit (@(nongnu packages emacs) emacs-org-roam-ui))
   ;;     (name "emacs-org-roam-ui")
   ;;     (version (git-version "0" revision commit))
   ;;     (source
   ;;      (origin
   ;;        (method git-fetch)
   ;;        (uri (git-reference
   ;;              (url "https://github.com/org-roam/org-roam-ui")
   ;;              (commit commit)))
   ;;        (file-name (git-file-name name version))
   ;;        (sha256
   ;;         (base32
   ;;          "1jfplgmx6gxgyzlc358q94l252970kvxnig12zrim2fa27lzmpyj"))))))
   ;; (hidden-package (@ (nrepl-python-channel) nrepl-python))
   (strings->packages
    "emacs-piem"
    "emacs-hl-todo"
    "emacs-consult-dir"
    "emacs-consult-org-roam"
    "emacs-restart-emacs"
    "emacs-csv-mode"
    "emacs-org-glossary"
    ;; "emacs-macrostep"
    ;; "emacs-ibrowse"
    "emacs-link-hint"
    ;; "emacs-forge"
    "emacs-origami-el"
    "emacs-emojify"
    "emacs-wgrep"
    "emacs-gptel"
    ;; "emacs-flycheck-package"
    ;; "python-lsp-server"
    "emacs-shackle"
    "emacs-combobulate"
    "emacs-org-pomodoro")))

(define %emacs-features
  (append
   (list
    (feature-emacs
     #:default-application-launcher? #t)
    (feature
     (name 'emacs-custom)
     (home-services-getter
      (const
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
    (feature-emacs-completion)
    (feature-emacs-corfu)
    (feature-emacs-vertico)
    (feature-emacs-pdf-tools)
    (feature-emacs-devdocs)
    (feature-emacs-dape)
    (feature-emacs-nov-el)
    (feature-emacs-comint)
    (feature-emacs-webpaste)
    (feature-emacs-help)
    (feature-emacs-all-the-icons)
    (feature-emacs-debbugs)
    (feature-emacs-flymake)
    (feature-emacs-xref)
    (feature-emacs-info)
    (feature-emacs-spelling
     #:spelling-program (@ (gnu packages hunspell) hunspell)
     #:spelling-dictionaries (strings->packages
                              "hunspell-dict-en"
                              "hunspell-dict-fr"))

    (feature-emacs-tramp)
    ;; I lost the work on emacs-dirvish...
    ;;(feature-emacs-dirvish
    ;; #:attributes '(file-size))
    (feature-emacs-eshell)
    (feature-emacs-eat)
    (feature-emacs-monocle
     #:olivetti-body-width 120)

    ;; (feature-emacs-telega)
    (feature-emacs-git)
    (feature-emacs-org
     #:org-directory (find-home "~")
     #:org-todo-keywords
     '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
       (sequence "DROP(D)" "|" "HOLD(h)")) ; waiting for someone to be ationable again
     #:org-tag-alist
     '((:startgroup)
       ;; Put mutually exclusive tags here
       (:endgroup)
       ("@home" . ?H)
       ("@freesoftware" . ?F)
       ("@work" . ?W)
       ("batch" . ?b) ; batchable task
       ("manage" . ?m) ; I'm responsible for the rythm of others
       ("organize" . ?o) ; better organization
       ("followup" . ?f))) ; someone is waiting on me to follow up

    (feature-emacs-org-agenda
     #:org-agenda-appt? #t
     #:org-agenda-custom-commands %org-agenda-custom-commands
     #:org-agenda-files "/home/graves/.cache/emacs/org-agenda-files")
    (feature-emacs-smartparens #:show-smartparens? #t)
    (feature-emacs-eglot)
    ;; (feature-emacs-geiser)
    (feature-emacs-graphviz)
    (feature-emacs-guix)
    (feature-emacs-tempel #:default-templates? #t)

    (feature-emacs-meow)
    (feature-emacs-undo-fu-session)
    (feature-emacs-elfeed #:elfeed-org-files '("~/resources/feeds.org"))
    (feature-emacs-org-ql)
    (feature-emacs-org-agenda-files-track)
    (feature-emacs-org-dailies
     #:org-dailies-directory "~/spheres/life/journal/"
     #:org-roam-dailies? #f)
    (feature-emacs-org-roam
     #:org-roam-directory "~/resources/roam/"
     #:org-roam-capture-templates  ;resource template is provided by citar
     '(("m" "main" plain "%?"
        :if-new (file+head "main/${slug}.org" "#+title: ${title}\n")
        :immediate-finish t
        :unnarrowed t)))

    (feature-emacs-citation
     #:citar-library-paths (list "~/resources/files/library")
     #:citar-notes-paths (list "~/resources/roam/references")
     #:global-bibliography (list "~/resources/biblio.bib" "~/resources/gen.bib"))

    (feature-go)

    (feature-emacs-elisp)
    (feature-emacs-power-menu)
    (feature-emacs-shell)
    (feature-vterm))
   (let* ((commit "1270f2581e47b97aa3c3b7eecfe3dc65bd24c412")
          (revision "4")
          (emacs-arei
           (package
             (inherit (@ (rde packages emacs-xyz) emacs-arei-latest))
             (name "emacs-arei")
             (version (git-version "0.9.6" revision commit))
             (source
              (origin
                (inherit (package-source emacs-arei))
                (uri (git-reference
                       (url "https://git.sr.ht/~abcdw/emacs-arei")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1xqhqibvf6d8ql3hl5l486wgy76m8z0a2ix28phbkn2jj4c79kkl")))))))
     (list (feature-guile #:emacs-arei emacs-arei
                          #:guile-ares-rs (@ (gnu packages guile-xyz) guile-ares-rs))
           (feature-python
            #:emacs-arei-python
            ((package-input-rewriting/spec
              `(("emacs-arei" . ,(const emacs-arei))))
             emacs-arei-python))))
   (list (feature-emacs-project))))


;;; Main features

(define %main-features
  (append
   (list
    (feature-shepherd)
    (feature-custom-services
     #:feature-name-prefix 'cups
     #:system-services
     (list (service (@ (gnu services cups) cups-service-type))))

    ;; (feature-docker)

    (feature-desktop-services)
    (feature-backlight #:step 5)
    (feature-pipewire)
    (feature-networking #:mdns? #t)
    ;; (feature-bluetooth)

    (feature-fonts
     #:default-font-size 14
     #:extra-font-packages
     (cons* font-gnu-unifont font-liberation
            (or (and=> (or@ (odf-dsfr packages fonts) font-marianne)
                       list)
                '())))

    (feature-foot
     #:default-terminal? #f
     #:backup-terminal? #t)
    (feature-zsh #:enable-zsh-autosuggestions? #t)
    (feature-bash)
    ;; (feature-transmission)

    (feature-compile)
    (feature-direnv)

    (feature-git
     #:sign-commits? #t
     #:git-sign-key
     "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINd9BmbuU3HS6pbCzCe1IZGxaHHDJERXpQRZZiRkfL3a"
     #:git-send-email? #t)

    ;; (feature-ledger)
    (feature-markdown)
    (feature-tex)
    (feature-mpv)
    ;; (feature-yt-dlp)
    (feature-imv)
    (feature-libreoffice)

    (feature-qemu)

    (feature-tmux)
    ;; (feature-ungoogled-chromium #:default-browser? #t)
    (feature-librewolf
     #:browser (hidden-package (@ (nongnu packages mozilla) firefox)))

    (feature-xdg
     #:xdg-user-directories-configuration
     (home-xdg-user-directories-configuration
      (download "~/.local/share/downloads")
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
      (hidden-package (@ (gnu packages version-control) git-lfs))
      (hidden-package (@ (gnu packages version-control) lfs-s3))
      (map
       hidden-package
       (strings->packages
       "hicolor-icon-theme" "adwaita-icon-theme" ; themes
       "alsa-utils"  ; sound
       "rsync" "zip"  ; "thunar"  ; documents
       "wev" "wlsunset" "cage"  ; wayland
       "recutils" "curl" "jq" "htop" ; utils
       "btrbk" ; snapshot btrfs subvolumes
       "atool" "unzip" ; provides generic extract tool aunpack
       "ccls"
       ;; "nerd-dictation-sox-wtype"
       ))
      )))
   %wm-features
   %emacs-features))


;;; Machine helpers
(define root-impermanence-btrfs-layout
  '((store  . "/gnu/store")
    (guix  . "/var/guix")
    (log  . "/var/log")
    (lib  . "/var/lib")
    (boot . "/boot")
    (NetworkManager . "/etc/NetworkManager")
    (ssh . "/etc/ssh"))) ; Needed for build offloading.

(define home-impermanence-para-btrfs-layout
  (append-map
   (lambda (subvol)
     (list
      (cons (string->symbol
             (if (string-prefix? "." subvol)
                 (string-drop subvol 1)
                 subvol))
            (string-append "/home/graves/" subvol))))
   '("projects" "spheres" "resources" "archives" ".local" ".cache")))

(define (machine-root-impermanence? machine)
  (not (assoc 'root (machine-btrfs-layout machine))))

(define (machine-home-impermanence? machine)
  (not (assoc 'home (machine-btrfs-layout machine))))

(define %mapped-device
  (let ((uuid (bytevector->uuid
               (string->uuid (machine-encrypted-uuid-mapped %current-machine)))))
    (and (uuid? uuid)
         (mapped-device
           (source uuid)
           (targets (list "enc"))
           (type luks-device-mapping)))))

(define root-fs
  (file-system
    (mount-point "/")
    (type (if (machine-root-impermanence? %current-machine)
              "tmpfs"
              "btrfs"))
    (device (if (machine-root-impermanence? %current-machine)
                "none"
                "/dev/mapper/enc"))
    (needed-for-boot? #t)
    (check? #f)))

(define home-fs
  (if (machine-home-impermanence? %current-machine)
      (file-system
        (mount-point "/home/graves")
        (type "tmpfs")
        (device "none")
        ;; User should have dir ownership.
        (options "uid=1000,gid=998")
        (dependencies (or (and=> %mapped-device list) '())))
      (file-system
        (mount-point "/home")
        (type "btrfs")
        (device "/dev/mapper/enc")
        (options "autodefrag,compress=zstd,subvol=home")
        (dependencies (or (and=> %mapped-device list) '())))))

(define get-btrfs-file-system
  (match-lambda
    ((subvol . mount-point)
     (file-system
       (type "btrfs")
       (device "/dev/mapper/enc")
       (mount-point mount-point)
       (options
        (format #f "~asubvol=~a"
                (if (string=? "/swap" mount-point)
                    "nodatacow,nodatasum,compress=no,"
                    "autodefrag,compress=zstd,")
                subvol))
       (needed-for-boot? (member mount-point
                                 '("/gnu/store" "/boot" "/var/guix")))
       (dependencies (append (or (and=> %mapped-device list) '())
                             (if (not (machine-root-impermanence? %current-machine))
                                 (list root-fs)
                                 '())
                             (if (and (not (machine-home-impermanence? %current-machine))
                                      (string-prefix? "/home/" mount-point))
                                 (list home-fs)
                                 '())))))))

(define swap-fs (get-btrfs-file-system '(swap . "/swap")))

(define btrfs-file-systems
  (append
   (list root-fs)
   (if (machine-home-impermanence? %current-machine)
       (list home-fs)
       '())
   (map get-btrfs-file-system
        (machine-btrfs-layout %current-machine))
   (list (file-system
           (mount-point "/boot/efi")
           (type "vfat")
           (device (machine-efi %current-machine))
           (needed-for-boot? #t))
         swap-fs)))

(use-modules (gnu home-services ssh)
             (gnu services security)
             (gnu services ssh)
             (guix scripts offload))

(define machine->build-machine
  (lambda (target-machine)
    #~(build-machine
       (name #$(machine-name target-machine))
       (systems (list #$(machine-architecture target-machine)))
       (user "graves")
       (host-key #$(machine-ssh-host-key target-machine))
       (private-key #$(machine-ssh-privkey-location %current-machine)))))

(define (machine->guix-pubkey target-machine)
  (plain-file
   (string-append (machine-name target-machine) "signing-key.pub")
   (format #f "\
(public-key
 (ecc
  (curve Ed25519)
  (q #~a#)))"
           (machine-guix-pubkey target-machine))))

(define machine->ssh-host
  (lambda (target-machine)
    (ssh-host
     (host (string-append (machine-name target-machine) ".local"))
     (options
      `((identity-file . ,(machine-ssh-privkey-location %current-machine)))))))

(define %machine-features
  (let* ((user-file-systems btrfs-file-systems
                            (partition
                             (lambda (fs)
                               ;; Or: has a file-system-dependency on HOME
                               (string-prefix?
                                "/home/"
                                ;; (get-value 'home-directory config)
                                (file-system-mount-point fs)))
                             btrfs-file-systems)))
    (append
     (list
      (feature-bootloader)
      (feature-file-systems
       #:mapped-devices (list %mapped-device)
       #:swap-devices
       (list (swap-space (target "/swap/swapfile")
                         (dependencies (list swap-fs))))
       #:file-systems btrfs-file-systems
       #:user-pam-file-systems user-file-systems
       #:base-file-systems (list %pseudo-terminal-file-system
                                 %debug-file-system
                                 (file-system
                                   (device "tmpfs")
                                   (mount-point "/dev/shm")
                                   (type "tmpfs")
                                   (check? #f)
                                   (flags '(no-suid no-dev))
                                   (options "size=80%")  ; This line has been changed.
                                   (create-mount-point? #t))
                                 %efivars-file-system
                                 %immutable-store))
      (feature-kernel
       #:kernel (if (null? (machine-firmware %current-machine))
                    linux-libre
                    (@ (nongnu packages linux) linux))
       #:initrd (or (or@ (nongnu system linux-initrd) microcode-initrd)
                    (@ (gnu system linux-initrd) base-initrd))
       #:initrd-modules
       (append (list "vmd") (@(gnu system linux-initrd) %base-initrd-modules))
       #:kernel-arguments  ; not clear, but these are additional to defaults
       (list "modprobe.blacklist=pcspkr" "rootfstype=tmpfs")
       #:firmware (machine-firmware %current-machine))
      (feature-base-services)
      (feature-custom-services
       #:feature-name-prefix 'more-substitutes
       #:system-services (list (force nonguix-service)
                               (force guix-science-service))))
     ;; Layout-specific features
     (if (machine-home-impermanence? %current-machine)
         (list
          (feature-user-pam-hooks
           #:on-login
           (program-file
            "guix-home-activate-on-login"
            #~(let* ((user (getenv "USER"))
                     (pw (getpw user))
                     (home (passwd:dir pw))
                     (profile
                       (string-append "/var/guix/profiles/per-user/" user)))
                (chdir home)
                (unless (file-exists? ".guix-home")
                  (symlink (string-append profile "/guix-home")
                           ".guix-home"))
                (unless (file-exists? ".config/guix/current")
                  (mkdir ".config")
                  (mkdir ".config/guix")
                  (symlink (string-append profile "/current-guix")
                           ".config/guix/current"))
                (system ".guix-home/activate")))))
         (list))
     ;; Device specific features
     (or (and-let* ((nvidia? (machine-nvidia? %current-machine))
                    (mesa-utils (or@ (gnu packages gl) mesa-utils)))
           (list (feature-sway-run-on-tty
                  #:sway-tty-number 1
                  ;; Currently not working properly on locking
                  ;; see https://github.com/NVIDIA/open-gpu-kernel-modules/issues/472
                  #:launch-arguments '("--unsupported-gpu"))
                 (feature-custom-services
                  #:feature-name-prefix 'machine
                  #:system-services
                  (list (simple-service 'nvidia-mesa-utils-package
                                        profile-service-type
                                        mesa-utils)))))
         (list (feature-sway-run-on-tty #:sway-tty-number 1)))
     ;; Machine-specific features
     (match (machine-name %current-machine)
       ("precision"
        (append
         (list (feature-host-info
                #:host-name "precision"
                #:timezone  "Europe/Paris"
                #:locale "fr_FR.utf8")
               ;; (feature-dictation)
               (feature-age
                #:age (hidden-package (@ (gnu packages golang-crypto) age))
                #:age-ssh-key (find-home "~/.local/share/ssh/id_encrypt"))
               (feature-security-token)
               (feature-password-store
                #:default-pass-prompt? #t
                #:password-store (@ (gnu packages password-utils) pass-age)
                #:password-store-directory (string-append cwd "/files/pass")
                #:remote-password-store-url "git@git.sr.ht:~ngraves/pass")
               (force %ssh-feature))
         (force %mail-features)
         ;; (list
          ;; (feature-custom-services
          ;;  #:feature-name-prefix 'build-machines
          ;;  #:system-services
          ;;  (list
          ;;   (simple-service
          ;;    'build-machines
          ;;    guix-service-type
          ;;    (guix-extension
          ;;     (build-machines
          ;;      (map machine->build-machine
          ;;           ;; %machines
          ;;           (filter machine-guix-pubkey
          ;;                   (delete %current-machine %machines)))))))))
         ))
       ("2325k55"
        (list (feature-host-info
               #:host-name "2325k55"
               #:timezone  "Europe/Paris"
               #:locale "fr_FR.utf8")
              (feature-ssh)))
       ("optiplex"
        (list (feature-host-info
               #:host-name "optiplex"
               #:timezone  "Europe/Paris"
               #:locale "fr_FR.utf8")
              (feature-ssh)))
       ("20xwcto1ww"
        (list (feature-host-info
               #:host-name "20xwcto1ww"
               #:timezone  "Europe/Paris"
               #:locale "en_US.utf8")
              (feature-age
               #:age (hidden-package (@ (gnu packages golang-crypto) age))
               #:age-ssh-key (find-home "~/.local/share/ssh/id_encrypt"))
              (feature-security-token)
              (feature-password-store
               #:default-pass-prompt? #t
               #:password-store (@ (gnu packages password-utils) pass-age)
               #:password-store-directory (string-append cwd "/files/pass")
               #:remote-password-store-url "git@git.sr.ht:~ngraves/pass")
              (force %ssh-feature)
              ))
       (_ '()))
     ;; Cross-machine features (ssh daemon + guix daemon offload)
     (list (feature-custom-services
            #:feature-name-prefix 'ssh-daemon
            #:home-services
            (list (simple-service
                   'ssh-server-authorized-key
                   home-files-service-type
                   `((".ssh/authorized_keys"
                      ,(plain-file "authorized-keys"
                                   (string-join
                                    (filter-map machine-ssh-pubkey %machines)
                                    "\n"))))))
            #:system-services
            (list (service openssh-service-type
                           (openssh-configuration
                            (openssh
                             (@ (gnu packages ssh) openssh-sans-x))
                            (allow-empty-passwords? #t)
                            (password-authentication? #f)))))
           (feature-custom-services
            #:feature-name-prefix 'ssh-build-machines
            #:home-services
            (list
             (simple-service
              'local-ssh-machines
              home-ssh-service-type
              (home-ssh-extension
               (extra-config (map machine->ssh-host %machines))))))
           (feature-custom-services
            #:feature-name-prefix 'build-machines-keys
            #:system-services
            (list
             (simple-service
              'build-machines
              guix-service-type
              (guix-extension
               (authorized-keys
                (map machine->guix-pubkey
                     (filter machine-guix-pubkey %machines)))))))))))


;;; rde-config and helpers for generating home-environment and
;;; operating-system records.

(define %config
  (let* ((config (rde-config
                  (features (append %user-features
                                    %main-features
                                    %machine-features))))
         (maybe->packages (or@ (guix-stack submodules)
                               submodules-dir->packages))
         (dev-packages (and=> maybe->packages
                              (cut <> "packages" #:git-fetch? #t))))
    (if dev-packages
        (override-rde-config-with-values config dev-packages)
        config)))

;; Dispatcher, self explanatory.
(match-let* ((((? (lambda (cmd)
                    (any (cut string-suffix? <> cmd)
                         (list "guile" "guix")))) . rest)
              (command-line))
             ((subcommand . rest) (if (null? rest) (cons '() '()) rest)))
  (match subcommand
    ('() %config)  ; (command-line) is guile, probably in ares.
    ("rde" %config)  ; See guix-rde channel.
    ("home" (rde-config-home-environment %config))
    ("system"
     (match-let (((action opts ...) rest))
       (match action
         ("vm" (force my-installation-os))
         ("image" (force my-installation-os))
         ;; sudo -E guix system CMD configuration.scm
         (_
          (or (and-let* ((nvidia (machine-nvidia? %current-machine))
                         (nonguix-transformation-nvidia
                          (or@ (nonguix transformations)
                               nonguix-transformation-nvidia))
                         (nvdb (or@ (nongnu packages nvidia) nvdb)))
                ((nonguix-transformation-nvidia #:driver nvdb)
                 (rde-config-operating-system %config)))
              (rde-config-operating-system %config))))))
    ("pull" ((@ (guix-stack channel-submodules) submodules-dir->channels)
             "channels"
             #:type '(branch . (or "origin/master" "origin/main"))))
    (_        (error "This configuration is configured for \
rde, home, pull, and system subcommands only!"))))



;;; Installation

;; More info : https://guix.gnu.org/manual/en/html_node/System-Installation.html
;;             https://wiki.systemcrafters.cc/guix/nonguix-installation-guide

;; Building the installation image: `guix system image configuration.scm'

;; Sending to USB stick: `sudo dd if=/gnu/store/{sha256}-disk-image of=/dev/sdX bs=1M status=progress'

;; Boot --> shell process --> Wifi: `rfkill unblock all && nmtui'
;; If SSL cert issues, check `date`, fix with `date -s 'YYYY-MM-DD HH:mm"

;; Setup partitions : `fdisk /dev/sda' (see https://guix.gnu.org/manual/en/guix.html#Disk-Partitioning)

;; Setup encryption :
;; mkfs.vfat -F32 /dev/<EFI partition>
;; cryptsetup luksFormat --type luks2 --pbkdf pbkdf2 /dev/<root partition>
;; cryptsetup open --type luks2 /dev/<root partition> enc
;; mkfs.btrfs /dev/mapper/enc
;; mount -t btrfs /dev/mapper/enc /mnt
;; for subvol in {boot,store,log,lib,guix,NetworkManager,ssh,btrbk_snapshots,swap}; do\
;;   btrfs subvolume create /mnt/${subvol};\
;; done
;; EITHER
;; btrfs subvolume create /mnt/root
;; btrfs subvolume create /mnt/home
;; OR (impermanence)
;; for subvol in {spheres,projects,resources,archives,local,cache}; do\
;;   btrfs subvolume create /mnt/${subvol};\
;; done
;; umount /mnt
;; mount -o subvol=root /dev/mapper/enc /mnt OR mount -t tmpfs none /mnt
;; for subvol in {boot,gnu/store,var/guix}; do\
;;   mkdir -p /mnt/${subvol} && mount -o compress=zstd,subvol=${subvol##*/} /dev/mapper/enc /mnt/${subvol};\
;; done
;; mkdir -p /mnt/boot/efi
;; mount /dev/<EFI partition> /mnt/boot/efi
;; mkdir -p /mnt/swap
;; mount -o nodatacow,nodatasum,subvol=swap /dev/mapper/enc /mnt/swap
;; btrfs filesystem mkswapfile --size 4g --uuid clear /mnt/swap/swapfile
;; If it fails, see underlying commands: https://btrfs.readthedocs.io/en/latest/Swapfile.html
;; swapon /mnt/swap/swapfile

;; Setup installation environment : `herd start cow-store /mnt'

;; Pull: `guix pull && hash guix' ; default channels in /etc/guix/channels.scm

;; Find encrypted partition UUIDs for configuration: `cryptsetup luksUUID /dev/<root partition>'

;; Init installation: `guix system init configuration.scm /mnt'

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

;;; Currently abandonned:
;; - system-connection services. see commit log.
