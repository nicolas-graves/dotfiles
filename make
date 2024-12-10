#!/run/current-system/profile/bin/env -S GUILE_LOAD_PATH=${HOME}/.config/guix/current/share/guile/site/3.0/:${GUILE_LOAD_PATH} guix repl --
!#
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2022-2024 Nicolas Graves <ngraves@ngraves.fr>

(use-modules
 ;; Modules for make.
 (srfi srfi-1) (srfi srfi-9 gnu) (srfi srfi-71) (srfi srfi-26) (git)
 (ice-9 match) (ice-9 textual-ports) (ice-9 popen) (ice-9 pretty-print)
 (guix gexp) (guix build utils) (guix channels) (guix records)
 (gnu home) (gnu system)
 (gnu system image) ((gnu image) #:hide (partition))
 ((rde features) #:select (rde-config-home-environment
                           rde-config-operating-system))
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
 (rde features system)
 (srfi srfi-1) (ice-9 popen) (ice-9 rdelim) (ice-9 match)
 (gnu system) (gnu system file-systems) (gnu system mapped-devices)
 (gnu system uuid)
 (gnu packages linux) (nongnu packages linux)
 (nongnu system linux-initrd)
 (guix memoization)

 ;; Modules for config.
 (ice-9 popen) (ice-9 pretty-print) (ice-9 rdelim)
 (srfi srfi-1) (ice-9 match)
 (gnu system)
 ((guix build utils) #:select (find-files))
 ((gnu packages) #:select (specification->package))
 ((rde packages) #:select (strings->packages))
 ((gnu services) #:select (simple-service etc-service-type service))
 ((guix download) #:select (url-fetch url-fetch/zipbomb))
 ((guix packages) #:select (origin base32 package))
 ((guix ui) #:select (with-error-handling))
 ((guix utils) #:select (readlink*))
 (guix build-system channel) ; (gnu packages package-management)
 (guix build-system font) (gnu packages fonts) (nonguix licenses)
 (guix gexp) (guix packages) (guix git-download) (guix git) (guix monads)
 (guix scripts system) (guix scripts system reconfigure) (gnu bootloader)

 ;; Modules for live config

 (gnu services networking)
 (gnu system file-systems)
 (gnu system install)
 (rde features)
 (rde features keyboard)
 (rde features system)
 (nongnu packages linux)
 (rde packages)
 (gnu services)

 ;; Other modules.
 (rde features)
 (rde features base)
 (rde features databases)
 (rde features emacs-xyz)
 (rde home services emacs)
 (rde features fontutils)
 (rde features golang)
 (rde features python)
 (rde features keyboard)
 (rde features linux)
 (rde features finance)
 (rde features mail)
 (rde features markup)
 (rde features networking)
 (rde features shells)
 (rde features shellutils)
 (rde features system)
 (rde features terminals)
 (rde features video)
 (rde features web-browsers)
 (rde features wm)
 (rde features password-utils)
 (rde features ssh)
 (rde features xdg)
 (rde features image-viewers)
 (contrib features emacs-xyz)
 (contrib features age)
 (nongnu packages linux)
 (gnu packages emacs-xyz)
 (gnu home services)
 (gnu home services guix)
 (gnu home services ssh)
 (guix derivations))

(define (sanitize-home-string str homedir)
  (if (string-prefix? "~" str)
      (string-append homedir (string-drop str 1))
      str))

(define (find-home str)
  (sanitize-home-string str (getenv "HOME")))

(define config-file
  (string-append (dirname (current-filename)) "/config.scm"))

(define %channels
  (delay
    (primitive-load
     (string-append (dirname (current-filename)) "/channels.scm"))))

(define btrbk-conf
  (string-append (dirname (current-filename)) "/hooks/btrbk.conf"))

(define* (read-line-recutils port #:optional str)
  "Read line in recutils format. For line:
1: equivalent to recutils, do not use argument STR.
2+: use argument STR to ensure the field."
  (if (or (not str)
          (let ((read (read-delimited " " port)))
            (and (string? read)
                 (string=? read (string-append str ":")))))
      (read-line port)
      #f))


;;; Nonguix helpers
(define nonguix-key
  (origin
    (method url-fetch)
    (uri "https://substitutes.nonguix.org/signing-key.pub")
    (sha256 (base32 "0j66nq1bxvbxf5n8q2py14sjbkn57my0mjwq7k1qm9ddghca7177"))))

(define %nonguix-feature
  (delay 
    (feature-base-services
     #:guix-substitute-urls
     (append (list "https://substitutes.nonguix.org")
             (@ (guix store) %default-substitute-urls))
     #:guix-authorized-keys
     (append (list nonguix-key)
             (@ (gnu services base) %default-authorized-guix-keys)))))


;;; Machine helpers
(define root-impermanence-btrfs-layout
  '((store  . "/gnu/store")
    (guix  . "/var/guix")
    (log  . "/var/log")
    (lib  . "/var/lib")
    (boot . "/boot")
    (NetworkManager . "/etc/NetworkManager")))

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

(define-record-type* <machine> machine make-machine
  machine?
  this-machine
  (name machine-name)                                    ; string
  (efi machine-efi)                                      ; file-system
  (encrypted-uuid-mapped machine-encrypted-uuid-mapped   ; maybe-uuid
                         (default #f))
  (btrfs-layout machine-btrfs-layout                     ; alist
                (default root-impermanence-btrfs-layout))
  (architecture machine-architecture                     ; string
                (default "x86_64-linux"))
  (firmware machine-firmware                             ; list of packages
            (default '()))
  (kernel-build-options machine-kernel-build-options     ; list of options
                        (default '()))
  (root-impermanence? machine-root-impermanence?         ; boolean
                      (thunked) 
                      (default 
                        (not (assoc 'root (machine-btrfs-layout this-machine)))))
  (home-impermanence? machine-home-impermanence?         ; boolean
                      (thunked) 
                      (default 
                        (not (assoc 'home (machine-btrfs-layout this-machine)))))
  (custom-services machine-custom-services               ; list of system-services
                   (default '())))

(define config-item->string
  (match-lambda
    ((option . 'm)
     (string-append option "=m"))
    ((option . #t)
     (string-append option "=y"))
    ((option . #f)
     (string-append option "=n"))
    ((option . string)
     (string-append option "=\"" string "\""))))

(define %machines
  (list
   (machine (name "Precision 3571")
            (efi "/dev/nvme0n1p1")
            (encrypted-uuid-mapped "92f9af3d-d860-4497-91ea-9e46a1dacf7a")
            (btrfs-layout (append '(;;(data . "/data")
                                    (btrbk_snapshots . "/btrbk_snapshots")
                                    (mozilla . "/home/graves/.mozilla")
                                    (zoom . "/home/graves/.zoom"))
                                  root-impermanence-btrfs-layout
                                  home-impermanence-para-btrfs-layout))
            (firmware (list linux-firmware))
            (custom-services (list ;; Currently not working properly on locking
                                   ;; see https://github.com/NVIDIA/open-gpu-kernel-modules/issues/472
                                   (service (@ (nongnu services nvidia) nvidia-service-type)
                                              ((@ (nongnu services nvidia) nvidia-configuration)
                                                (driver (@@ (nongnu packages nvidia) mesa/fake-beta))
                                                (firmware (@ (nongnu packages nvidia) nvidia-firmware-beta))
                                                (module (@ (nongnu packages nvidia) nvidia-module-beta))))
                                   (simple-service 'nvidia-mesa-utils-package
                                                   profile-service-type
                                                   (list (@ (gnu packages gl) mesa-utils))))))
   (machine (name "20AMS6GD00")
            (efi "/dev/sda1")
            (encrypted-uuid-mapped "a9319ee9-f216-4cad-bfa5-99a24a576562"))
   (machine (name "2325K55")
            (efi "/dev/sda1")
            (encrypted-uuid-mapped "824f71bd-8709-4b8e-8fd6-deee7ad1e4f0")
            (btrfs-layout (cons* '(home . "/home") root-impermanence-btrfs-layout))
            (firmware (list iwlwifi-firmware)))
   ;; Might use r8169 module but it works fine without, use linux-libre then.
   (machine (name "OptiPlex 3020M")
            (efi "/dev/sda1")
            (encrypted-uuid-mapped "be1f04af-dafe-4e1b-8e8b-a602951eeb35")
            (btrfs-layout (cons* '(home . "/home") root-impermanence-btrfs-layout)))))

(define (get-hardware-features)

  (define %machine-name ;; hardcoded current machine name
    (call-with-input-file "/sys/devices/virtual/dmi/id/product_name"
      read-line))

  (define %current-machine
    (find (lambda (in) (equal? %machine-name (machine-name in)))
          %machines))

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

  (define get-btrfs-file-system
    (memoize 
      (match-lambda
        ((subvol . mount-point)
          (file-system
           (type "btrfs")
           (device "/dev/mapper/enc")
           (mount-point mount-point)
           (options
            (format #f "~asubvol=~a"
                    (if (string=? "/swap" mount-point)
                        "nodatacow,nodatasum,"
                        "autodefrag,compress=zstd,")
                    subvol))
           (needed-for-boot? (member mount-point 
                                     '("/" "/gnu/store" "/var/guix" "/boot")))
           (dependencies (append (or (and=> %mapped-device list) '())
                                 (if (not (machine-root-impermanence? %current-machine))
                                     (list root-fs)
                                     '())
                                 (if (and (not (machine-home-impermanence? %current-machine))
                                          (string-prefix? "/home/" mount-point))
                                     (list (get-btrfs-file-system '(home . "/home")))
                                     '()))))))))

  (define btrfs-file-systems
    (map get-btrfs-file-system
         (machine-btrfs-layout %current-machine)))

  (define swap-fs (get-btrfs-file-system '(swap . "/swap")))

  (define my-linux
    (if (null? (machine-firmware %current-machine))
        linux-libre-6.11
        linux-6.11))

  (define btrfs-file-systems
    (append
     (list root-fs)
     btrfs-file-systems
     (list (file-system
             (mount-point "/boot/efi")
             (type "vfat")
             (device (machine-efi %current-machine))
             (needed-for-boot? #t))
           swap-fs)))

  (append 
    (list
     (feature-bootloader)
     (feature-file-systems
      #:mapped-devices (list %mapped-device)
      #:swap-devices
      (list (swap-space (target "/swap/swapfile")
                        (dependencies (list swap-fs))))
      #:file-systems btrfs-file-systems
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
      #:kernel my-linux
      #:initrd microcode-initrd
      #:initrd-modules
      (append (list "vmd") (@(gnu system linux-initrd) %base-initrd-modules))
      #:kernel-arguments  ; not clear, but these are additional to defaults
      (list
       ;; "modprobe.blacklist=pcspkr" "rootfstype=tmpfs"
       ;; Currently not working properly on locking
       ;; see https://github.com/NVIDIA/open-gpu-kernel-modules/issues/472
       "modprobe.blacklist=pcspkr,nouveau" "rootfstype=tmpfs"
       ;; "nvidia_drm.modeset=1" "nvidia_drm.fbdev=1"
       )
      #:firmware (machine-firmware %current-machine)))
      (let ((services (machine-custom-services %current-machine)))
        (if (null? services)
          '()
          (list (feature-custom-services
                 #:feature-name-prefix 'machine
                 #:system-services services))))))


;;; Live systems.
(define my-installation-os
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
       #:firmware (list linux-firmware))
      (feature-base-packages
       #:system-packages
       (strings->packages "git" "zip" "unzip" "curl"
                          "exfat-utils" "fuse-exfat" "ntfs-3g"))
      (feature-custom-services
       #:feature-name-prefix 'live
       #:system-services
       (list
        ;; (simple-service
        ;;  'channels-and-sources
        ;;  etc-service-type
        ;;  `(("channels.scm" ,(local-file "/home/graves/.config/guix/channels.scm"))
        ;;    ("guix-sources" ,(local-file "/home/graves/spheres/info/guix" #:recursive? #t))
        ;;    ("nonguix-sources" ,(local-file "/home/graves/spheres/info/nonguix" #:recursive? #t))
        ;;    ("rde-sources" ,(local-file "/home/graves/spheres/info/rde" #:recursive? #t))))
        (service wpa-supplicant-service-type)
        (service network-manager-service-type)
        (service (@@ (gnu system install) cow-store-service-type) 'mooh!)))
      (feature-base-services
       #:guix-substitute-urls (list "https://substitutes.nonguix.org")
       #:guix-authorized-keys (list nonguix-key)))))))


;;; Pull scripts
(define* (make-pull #:key (args (list "--allow-downgrades"
                                      "--disable-authentication")))
  "Call `make-force-pull' if there are new commits in source directories."
  (let* ((channels instances
                   (partition channel? (force %channels)))
         (next-channels (append
                         channels
                         (map
                          (lambda (instance)
                            (let ((this-channel
                                   (channel-instance-channel instance)))
                              (if (file-like?
                                   (channel-instance-checkout instance))
                                  (channel (inherit this-channel)
                                           (commit #f))
                                  this-channel)))
                          instances))))
    (if
     (every (lambda (x)
              (let* ((previous-channel (sexp->channel
                                        (cadar (manifest-entry-properties x))))
                     (next-channel (first
                                    (filter
                                     (lambda (channel)
                                       (eq? (channel-name channel)
                                            (channel-name previous-channel)))
                                     next-channels))))
                (string= (channel-commit previous-channel)
                         (or (channel-commit next-channel)
                             (let ((url (channel-url next-channel)))
                               (and (file-exists? url)
                                    (oid->string
                                     (object-id
                                      (revparse-single
                                       (repository-open url)
                                       (channel-branch next-channel))))))
                             (make-string 40 #\0)))))
            (manifest-entries
             (profile-manifest (find-home "~/.config/guix/current"))))
     (display "Pull: Nothing to be done.\n")
     (make-force-pull #:args args
                      #:channels channels
                      #:instances instances))))

(define* (make-force-pull #:key
                          (args (list "--allow-downgrades"
                                      "--disable-authentication"))
                          (channels '())
                          (instances '()))
  (eval
   `(begin
      (reload-module (current-module))

      (define (no-arguments arg _)
        (leave (G_ "~A: extraneous argument~%") arg))

      (with-error-handling
        (with-git-error-handling
         (let* ((opts         (parse-command-line ',args %options
                               (list %default-options)
                               #:argument-handler no-arguments))
                (substitutes? (assoc-ref opts 'substitutes?))
                (dry-run?     (assoc-ref opts 'dry-run?))
                (profile      (or (assoc-ref opts 'profile) %current-profile))
                (current-channels (profile-channels profile))
                (validate-pull    (assoc-ref opts 'validate-pull))
                (authenticate?    (assoc-ref opts 'authenticate-channels?)))
           (cond
            ((assoc-ref opts 'query)
             (process-query opts profile))
            ((assoc-ref opts 'generation)
             (process-generation-change opts profile))
            (else
             ;; Bail out early when users accidentally run, e.g., ’sudo guix pull’.
             ;; If CACHE-DIRECTORY doesn't yet exist, test where it would end up.
             (validate-cache-directory-ownership)

             (with-store store
               (with-status-verbosity (assoc-ref opts 'verbosity)
                 (parameterize ((%current-system (assoc-ref opts 'system))
                                (%graft? (assoc-ref opts 'graft?)))
                   (with-build-handler (build-notifier #:use-substitutes?
                                                       substitutes?
                                                       #:verbosity
                                                       (assoc-ref opts 'verbosity)
                                                       #:dry-run? dry-run?)
                     (set-build-options-from-command-line store opts)
                     (ensure-default-profile)
                     (honor-x509-certificates store)

                     ;; XXX: Guix source code change.
                     (let* ((instances (append
                                        (latest-channel-instances
                                         store ',channels
                                         #:current-channels current-channels
                                         #:validate-pull validate-pull
                                         #:authenticate? authenticate?)
                                        ',instances)))
                       ;; XXX: End of Guix source code change.
                       (format (current-error-port)
                               (N_ "Building from this channel:~%"
                                   "Building from these channels:~%"
                                   (length instances)))
                       (for-each (lambda (instance)
                                   (let ((channel
                                          (channel-instance-channel instance)))
                                     (format (current-error-port)
                                             "  ~10a~a\t~a~%"
                                             (channel-name channel)
                                             (channel-url channel)
                                             (string-take
                                              (channel-instance-commit instance)
                                              7))))
                                 instances)
                       (with-profile-lock profile
                         (run-with-store store
                           (build-and-install instances profile))))))))))))))
   (resolve-module '(guix scripts pull) #:ensure #f)))


;;; System scripts
;; With this version, you can simply run ./make all and only be prompted with
;; the password on recompilation within emacs when necessary, with no remaning
;; sudo subprocess on exit.
(define %system-profile
  ;; The system profile.
  (string-append (@(guix config) %state-directory) "/profiles/system"))

(define* (sudo-eval exp
                    #:key load-path load-compiled-path)
  (let* ((cmd (format #f "sudo guile~a~a -c \"~a\""
                      (if (and load-path (not (null? load-path)))
                          (format #f " -L ~a" (string-join load-path ":"))
                          "")
                      (if (and load-compiled-path (not (null? load-compiled-path)))
                          (format #f " -C ~a" (string-join load-compiled-path ":"))
                          "")
                      (string-join (string-split exp #\") "\\\"")))
         (port (open-input-pipe (pk 'cmd cmd)))
         (result (pk 'result (get-string-all port))))
    (close-pipe port)
    (if (and (not (string-null? result)) (not (equal? "#<unspecified>" result)))
        (pk 'result (read (open-input-string result)))
        '())))

(define (local-eval exp)
  "Evaluate EXP, a G-Expression, in-place in a sudo pipe."
  (mlet* %store-monad ((lowered (lower-gexp exp))
                       (_ ((@ (guix derivations) built-derivations)
                           (lowered-gexp-inputs lowered))))
    (return
     (sudo-eval (format #f "~s" (lowered-gexp-sexp lowered))
                #:load-path (lowered-gexp-load-path lowered)
                #:load-compiled-path (lowered-gexp-load-compiled-path lowered)))))

(define* (make-system #:optional rest)
  (if (equal? rest (list "--allow-downgrades"))
      (with-store store
        (run-with-store store
          (reconfigure-system
           (rde-config-operating-system (primitive-load config-file))
           (lambda ()
             (system* "sudo" "btrbk" "-c" btrbk-conf "run")))))
      (begin
        (apply system*
               (cons* "sudo" "-E" "guix" "system" "reconfigure" "make" rest))
        (system* "sudo" "btrbk" "-c" btrbk-conf "run"))))

(define* (reconfigure-system os #:optional reconfigure-hook)
  "This monadic function reconfigures %guix-system from an operating system
OS.  Compared to the code in Guix, it avoids trying to reconfigure system if the
calculated profile is the actual profile, and enables a higher level function
to ask for a password if and only if it is necessary.

Optionally run RECONFIGURE-HOOK, a function to be run just before
reconfiguring the system in the case it is necessary. This is useful for
saving snapshots for instance."
  (mlet* %store-monad
      ((%         -> (display "Reconfiguring system...\n"))
       (os-drv    (operating-system-derivation os))
       (future-os -> (derivation->output-path os-drv)))
    (if (equal? future-os (readlink "/run/current-system"))
        (begin
          (display "System: Nothing to be done.\n")
          (return #f))
        (begin
          (when reconfigure-hook (reconfigure-hook))
          (format #t "activating system...~%")
          (let* ((bootcfg (operating-system-bootcfg
                           os (map boot-parameters->menu-entry
                                   (@@ (guix scripts system)
                                       profile-boot-parameters))))
                 (bootloader (operating-system-bootloader os)))
            (mbegin %store-monad
              (switch-to-system local-eval os)
              (install-bootloader local-eval bootloader bootcfg #:target "/")
              (return
               (format #t "bootloader successfully installed on '~a'~%"
                       (bootloader-configuration-targets bootloader)))
              ((@@ (guix scripts system) with-shepherd-error-handling)
               (upgrade-shepherd-services local-eval os)
               (return (format #t "\
To complete the upgrade, run 'herd restart SERVICE' to stop,
upgrade, and restart each service that was not automatically restarted.\n"))
               (return (format #t "\
Run 'herd status' to view the list of services on your system.\n")))))))))


;;; Home scripts.
;; TODO make home-init target in case of from scratch installation
(define %guix-home
  (string-append %profile-directory "/guix-home"))

;; TODO Make logging pretty like in Guix.
(define* (reconfigure-home he)
  "This monadic function reconfigures %guix-home from a home environment HE.
Compared to the code in Guix, it avoids trying to reconfigure home if the
calculated profile is the actual profile."
  (mlet* %store-monad
      ((%           -> (display "Reconfiguring home...\n"))
       (transf-he   -> (home-environment-with-provenance he))
       (he-drv         (home-environment-derivation transf-he))
       (he-out-path -> (derivation->output-path he-drv)))
    (if (equal? he-out-path (readlink (readlink %guix-home)))
        (begin
          (display "Home: Nothing to be done.\n")
          (return #f))
        (mbegin %store-monad
          (built-derivations (list he-drv))
          (let* ((number (generation-number %guix-home))
                 (generation (generation-file-name %guix-home (+ 1 number))))
            (switch-symlinks generation he-out-path)
            (switch-symlinks %guix-home generation)
            (setenv "GUIX_NEW_HOME" he-out-path)
            (primitive-load (string-append he-out-path "/activate"))
            (setenv "GUIX_NEW_HOME" #f)
            (return he-out-path))))))

(define* (make-home #:optional rest)
  (if (equal? rest (list "--allow-downgrades"))
      (with-store store
        (run-with-store store
          (reconfigure-home
           (rde-config-home-environment (primitive-load config-file)))))
      (apply system*
             (cons* "guix" "home" "reconfigure" "make"
                    "--keep-failed" "--fallback" rest))))


;;; "make all"
(define* (make-all #:optional rest)
  (make-pull)
  (let ((config (primitive-load config-file)))
    (with-store store
      (run-with-store store
        ((@ (guix monads) mbegin) %store-monad
          (reconfigure-system (rde-config-operating-system config))
          (reconfigure-home (rde-config-home-environment config)))))))

;;; Dispatcher
(match-let (((bin args ...) (command-line)))
  (match bin
    ("./make"  ; called from ./make
     (match-let (((str rest ...) args))
       (match str
         ("kernel" my-linux)
         ("config" (primitive-load config-file))
         ("repl" (apply (@(guix scripts repl) guix-repl) '("-i")))
         ("pull" (make-pull))
         (_ (eval-string
             (string-append "(make-" str
                            " (list \"" (string-join
                                         (cons* "--allow-downgrades" rest)
                                         "\" \"")
                            "\"))"))))))
    ((? (cute string-suffix? "guix" <>))  ; called from guix
     (match-let (((str rest ...) args))
       (match str
         ("home" (rde-config-home-environment (primitive-load config-file)))
         ("system" (match-let (((action opts ...) rest))
                     (match action
                       ("image" my-installation-os)
                       (_  (rde-config-operating-system
                            (primitive-load config-file))))))
         (_        (error "\
guix with make argument is configured for home and system only")))))
    (_ (primitive-load config-file))))

;; Local Variables:
;; compile-command: "GUILE_LOAD_PATH=${PWD}/channels/nonguix:${PWD}/channels/rde/src:${GUILE_LOAD_PATH} guild compile make"
;; mode: scheme
;; End:
