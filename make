#!/usr/bin/env -S GUILE_LOAD_PATH=${HOME}/.config/guix/current/share/guile/site/3.0/:${GUILE_LOAD_PATH} GUILE_LOAD_COMPILED_PATH=${HOME}/.config/guix/current/lib/guile/3.0/site-ccache/:${GUILE_LOAD_COMPILED_PATH} guix repl --
!#
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2022,2023 Nicolas Graves <ngraves@ngraves.fr>

;; Modules for make.
(use-modules
 (srfi srfi-1) (srfi srfi-9 gnu) (srfi srfi-71) (srfi srfi-26) (git)
 (ice-9 match) (ice-9 textual-ports) (ice-9 popen)
 (guix monads) (guix gexp) (guix build utils)
 (gnu home) (gnu system)
 ((rde features) #:select (sanitize-home-string
                           rde-config-home-environment
                           rde-config-operating-system))
 ((guix profiles) #:select (manifest-entries
                            manifest-entry-properties
                            %profile-directory
                            profile-manifest
                            generation-number
                            generation-file-name))
 ((guix store) #:select (with-store run-with-store
                         %store-monad mapm/accumulate-builds)))

(define (find-home str)
         (sanitize-home-string str (getenv "HOME")))

(define config-file
  (string-append (dirname (current-filename)) "/README.scm"))

;;; Code blocks
(define code-blocks
  ;; Modules for config.
  `((config
     .
     (begin
       (use-modules
        ;; Guile+Guix libraries.
        (ice-9 popen) (ice-9 pretty-print) (ice-9 rdelim)
        (srfi srfi-1) (ice-9 match)
        (gnu system)
        ((guix build utils) #:select (find-files))
        ((gnu packages) #:select (specification->package))
        ((rde packages) #:select (strings->packages))
        ((gnu services) #:select (simple-service etc-service-type service))
        ((guix download) #:select (url-fetch url-fetch/zipbomb))
        ((guix packages) #:select (origin base32 package))
        (guix gexp) (guix packages) (guix git-download)

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
        (rde features image-viewers)
        (contrib features emacs-xyz)
        (contrib features age)
        (nongnu packages linux)
        (guix records))
       (define (find-home str)
         (sanitize-home-string str (getenv "HOME")))
       (include ,config-file)))
    ;; Machine helpers.
    (machine
     .
     (begin
       (use-modules (guix records)
                    (srfi srfi-1)
                    (rde features system)
                    (ice-9 popen)
                    (ice-9 rdelim)
                    (ice-9 match)
                    (gnu system)
                    (nongnu packages linux)
                    (nongnu system linux-initrd)
                    (gnu system file-systems)
                    (gnu system mapped-devices))

       (define-record-type* <machine> machine make-machine
         machine?
         this-machine
         (name machine-name) ;string
         (efi machine-efi) ;file-system
         (uuid-mapped machine-uuid-mapped) ;uuid
         (firmware machine-firmware (default '())) ;list of packages
         (features machine-features (default '()))) ;list of features

       (define (get-hardware-features machines)

         (define (get-mapped-device local-machine)
           (mapped-device
            (source (uuid (machine-uuid-mapped local-machine)))
            (targets (list "enc"))
            (type luks-device-mapping)))

         (define (get-swap-fs local-machine)
           (file-system
            (type "btrfs")
            (device "/dev/mapper/enc")
            (mount-point "/swap")
            (options "subvol=swap")
            (dependencies (list (get-mapped-device local-machine)))))

         (define (get-home-fs local-machine)
           (file-system
            (type "btrfs")
            (device "/dev/mapper/enc")
            (mount-point "/home")
            (options "autodefrag,compress=zstd,subvol=home")
            (dependencies (list (get-mapped-device local-machine)))))

         (define (get-boot-fs local-machine)
           (file-system
            (type "btrfs")
            (device "/dev/mapper/enc")
            (mount-point "/boot")
            (options "autodefrag,compress=zstd,subvol=boot")
            (needed-for-boot? #t)
            (dependencies (list (get-mapped-device local-machine)))))

         (define (get-btrfs-file-system local-machine)
           (append
            (list (file-system
                   (mount-point "/")
                   (type "tmpfs")
                   (device "none")
                   (needed-for-boot? #t)
                   (check? #f)))
            (map
             (match-lambda
               ((subvol . mount-point)
                (file-system
                 (type "btrfs")
                 (device "/dev/mapper/enc")
                 (mount-point mount-point)
                 (options
                  (format
                   #f "autodefrag,compress=zstd,subvol=~a" subvol))
                 (needed-for-boot? (or (string=? "/gnu/store" mount-point)
                                       (string=? "/var/guix" mount-point)))
                 (dependencies (append (list (get-mapped-device local-machine))
                                       (if (string-prefix? "/home/graves" mount-point)
                                           (list (get-home-fs local-machine))
                                           '()))))))
             '((store  . "/gnu/store")
               (guix  . "/var/guix")
               (log  . "/var/log")
               (home . "/home")
               (data . "/data")
               (lib  . "/var/lib")
               (NetworkManager . "/etc/NetworkManager")
               (btrbk_snapshots . "/btrbk_snapshots")
               (spheres  . "/home/graves/spheres")
               (projects  . "/home/graves/projects")
               (resources  . "/home/graves/resources")
               (archives  . "/home/graves/archives")
               (zoom . "/home/graves/.zoom")
               (local . "/home/graves/.local")
               (cache . "/home/graves/.cache")
               (mozilla . "/home/graves/.mozilla")
               (guix-config . "/home/graves/.config/guix")))
            (list (file-system
                   (mount-point "/boot/efi")
                   (type "vfat")
                   (device (machine-efi local-machine))
                   (needed-for-boot? #t))
                  (get-boot-fs local-machine)
                  (get-swap-fs local-machine))))

         ;; This function looks up the hardcoded value of the current machine name.
         (define (get-machine-name)
           (call-with-input-file "/sys/devices/virtual/dmi/id/product_name"
             read-line))

         (define (current-machine? local-machine)
           (if (equal? (machine-name local-machine) (get-machine-name))
               local-machine
               #f))

         (define (current-machine machines)
           (car (filter-map current-machine? machines)))

         (let ((machine (current-machine machines)))
           (append
            (machine-features machine)
            (list
             (feature-bootloader)
             (feature-file-systems
              #:mapped-devices (list (get-mapped-device machine))
              #:swap-devices
              (list (swap-space (target "/swap/swapfile")
                                (dependencies (list (get-swap-fs machine)))))
              #:file-systems (get-btrfs-file-system machine))
             (feature-kernel
              #:kernel linux
              #:initrd microcode-initrd
              #:initrd-modules
              (append (list "vmd") (@(gnu system linux-initrd) %base-initrd-modules))
              #:kernel-arguments
              (list (string-append
                     "modprobe.blacklist="
                     (string-join
                      (cons* "pcspkr" (@@ (gnu system) %default-modprobe-blacklist)))
                     "quiet" "rootfstype=tmpfs"))
              #:firmware (machine-firmware machine))))))))
    ;; Nonguix features
    (nonguix
     .
     (begin
       (use-modules (nongnu system linux-initrd)
                    (rde features base)
                    ((guix download) #:select (url-fetch))
                    ((guix packages) #:select (origin base32)))

       (define nonguix-key
         (origin
          (method url-fetch)
          (uri "https://substitutes.nonguix.org/signing-key.pub")
          (sha256 (base32 "0j66nq1bxvbxf5n8q2py14sjbkn57my0mjwq7k1qm9ddghca7177"))))

       (define %nonguix-feature
         (feature-base-services
          #:guix-substitute-urls
          (append (list "https://substitutes.nonguix.org")
                  (@ (guix store) %default-substitute-urls))
          #:guix-authorized-keys
          (append (list nonguix-key)
                  (@ (gnu services base) %default-authorized-guix-keys))))))
    ;; Channels scripts
    (channels
     .
     (begin
       (use-modules (guix channels) (guix records)
                    (srfi srfi-1)
                    ((rde features) #:select (sanitize-home-string))
                    (ice-9 popen)
                    (ice-9 pretty-print))
       (define (find-home str)
         (sanitize-home-string str (getenv "HOME")))

       (define-record-type* <channel> channel make-channel
         channel?
         this-channel
         (name channel-name) ;string or symbol
         (url channel-url) ;string
         (commit channel-commit (default "master")) ;string
         (introduction channel-introduction)) ;channel introduction, probably a plist here.

       (define %default-guix
         (channel
          (name 'guix)
          (url "https://git.savannah.gnu.org/git/guix.git")
          (introduction
           '(make-channel-introduction
             "9edb3f66fd807b096b48283debdcddccfea34bad"
             (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))

       (define %default-nonguix
         (channel
          (name 'nonguix)
          (url "https://gitlab.com/nonguix/nonguix.git")
          (introduction
           '(make-channel-introduction
             "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
             (openpgp-fingerprint
              "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))

       (define %default-rde
         (channel
          (name 'rde)
          (url "https://git.sr.ht/~abcdw/rde")
          (introduction
           '(make-channel-introduction
             "257cebd587b66e4d865b3537a9a88cccd7107c95"
             (openpgp-fingerprint
              "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0")))))

       ;; Here you will also define default channels.
       ;; In the config, you inherit from them to make the actual channels.
       (define hexa-char-set
         (string->char-set "0123456789abcdef"))

       (define (commit? str)
         (and (eq? (string-length str) 40)
              (char-set-every
               (lambda (ch) (char-set-contains? hexa-char-set ch))
               (string->char-set str))))

       ;; This function generates the content of the channels file from a channels list.
       (define* (make-channels channel-list
                               #:key (file-or-port "~/.config/guix/channels.scm"))
         (let ((to-print (append-map
                          (lambda (ch) `((channel
                                          (name ',(channel-name ch))
                                          (url ,(find-home (channel-url ch)))
                                          ,@(if (commit? (channel-commit ch))
                                                `((commit ,(channel-commit ch)))
                                                `((branch ,(channel-commit ch))))
                                          (introduction ,(channel-introduction ch)))))
                          channel-list)))
           (if (port? file-or-port)
               (pretty-print `(list ,@to-print) file-or-port)
               (with-output-to-file (find-home file-or-port)
                 (lambda _
                   (pretty-print `(list ,@to-print)))))))))
    (config-channels
     .
     ,(with-input-from-file config-file
        (lambda () (read))))
    (live
     .
     (begin
       (use-modules (gnu services networking)
                    (gnu system file-systems)
                    (gnu system install)
                    (rde features)
                    (rde features keyboard)
                    (rde features system)
                    (nongnu packages linux)
                    (rde packages)
                    (gnu services))
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
             (feature-file-systems
              #:file-systems %base-live-file-systems)
             (feature-kernel
              #:kernel linux
              #:firmware (list linux-firmware))
             (feature-base-packages
              #:system-packages
              (strings->packages "vim" "git" "zip" "unzip" "make" "curl"
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
               (service network-manager-service-type)))
             (feature-base-services
              #:guix-substitute-urls (list "https://substitutes.nonguix.org")
              #:guix-authorized-keys (list nonguix-key)))))))))))

(define-syntax-rule (with-blocks blocks str)
  (let ((fmtstr (string-join (make-list (length blocks) "~s") " ")))
    (apply format #f (string-append "(begin " fmtstr " ~a)")
           (append (map (lambda (bn) (assoc-ref code-blocks bn)) blocks)
                   (list str)))))


;;; Live systems.
(define* (make-live-install #:optional rest)
  (apply
   (@ (guix scripts system) guix-system)
   (cons* "image"
          (string-append
           "--expression="
           (with-blocks '(nonguix live) "my-installation-os"))
          "--image-size=14G"
          rest)))


;;; Pull scripts
;; TODO pull script doesn't work properly with pinned commits.
(define* (make-pull #:optional rest)
  "Call function `make-force-pull' if there are new commits in source directories."
  (if
   (reduce (lambda (x y) (and x y)) #f
           (map
            (lambda (x)
              (let* ((elts (cdadar (manifest-entry-properties x)))
                     (repository (repository-open (car (assoc-ref elts 'url))))
                     (commit (oid->string
                              (object-id
                               (revparse-single
                                repository
                                (car (assoc-ref elts 'branch)))))))
                (string= commit (car (assoc-ref elts 'commit)))))
            (manifest-entries
             (profile-manifest (find-home "~/.config/guix/current")))))
   (display "Pull: Nothing to be done.\n")
   (make-force-pull rest)))

(define* (make-force-pull #:optional rest)
  (apply (@ (guix scripts pull) guix-pull)
         (cons* "--disable-authentication" "--allow-downgrades"
                (string-append "--channels=" (find-home "~/.config/guix/channels.scm"))
                (string-append "--profile="  (find-home "~/.config/guix/current"))
                rest)))


;;; System scripts
;; With this version, you can simply run ./make all and only be prompted with
;; the password on recompilation within emacs when necessary, with no remaning
;; sudo subprocess on exit.
(define %system-profile
  ;; The system profile.
  (string-append (@(guix config) %state-directory) "/profiles/system"))

;; The issue with guix system init is that it doesn't seem to accept
;; --expression so we must use a file in the meantime.
(define* (make-system-init #:optional rest)
  (with-output-to-file "config"
    (lambda () (display (with-blocks '(channels machine nonguix config)
                                     "(rde-config-operating-system %config)")))))

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
      (reconfigure-system
       (eval-string
        (with-blocks '(nonguix channels machine config)
                     "(rde-config-operating-system %config)")))
      (apply system*
             (cons* "sudo" "-E" "guix" "system" "reconfigure"
                    (string-append
                     "--expression="
                     (with-blocks '(channels machine nonguix config)
                                  "(rde-config-operating-system %config)"))
                    rest))))

(define* (reconfigure-system os)
  (with-store store
    (run-with-store store
      (mlet* %store-monad
          ((os-drv (operating-system-derivation os))
           (future-os -> ((@ (guix derivations) derivation->output-path) os-drv)))
        (if (equal? future-os (readlink "/run/current-system"))
            (begin
              (display "System: Nothing to be done.\n")
              (return #f))
            (begin
              (system* "sudo" "btrbk" "-c" "/home/graves/spheres/info/dots/hooks/btrbk.conf" "run")
              (format #t "activating system...~%")
              (let* ((bootcfg (operating-system-bootcfg
                               os (map boot-parameters->menu-entry
                                       ((@ (guix scripts system) profile-boot-parameters)))))
                     (bootloader (operating-system-bootloader os)))
                (mbegin %store-monad
                  ((@ (guix scripts system reconfigure) switch-to-system) local-eval os)
                  ((@ (guix scripts system reconfigure) install-bootloader)
                   local-eval bootloader bootcfg #:target "/")
                  (return
                   (format #t "bootloader successfully installed on '~a'~%"
                           ((@ (gnu bootloader) bootloader-configuration-targets)
                            bootloader)))
                  ((@ (guix scripts system reconfigure) upgrade-shepherd-services)
                   local-eval os)
                  (return (format #t "\
To complete the upgrade, run 'herd restart SERVICE' to stop,
upgrade, and restart each service that was not automatically restarted.\n"))
                  (return (format #t "\
Run 'herd status' to view the list of services on your system.\n"))))))))))


;;; Home scripts.
;; TODO make home-init target in case of from scratch installation
(define %guix-home
  (string-append %profile-directory "/guix-home"))

(define* (reconfigure-home he)
  (with-store store
    (run-with-store store
      (mlet* %store-monad
          ((he-drv (home-environment-derivation he))
           (drvs (mapm/accumulate-builds lower-object (list he-drv)))
           (%    ((@ (guix derivations) built-derivations) drvs))
           (he-out-path -> ((@ (guix derivations) derivation->output-path) he-drv)))
        (if (equal? he-out-path (readlink (readlink %guix-home)))
            (begin
              (display "Home: Nothing to be done.\n")
              (return #f))
            (let* ((number (generation-number (pk 'home %guix-home)))
                   (generation (generation-file-name
                                %guix-home (+ 1 number))))

              (switch-symlinks generation he-out-path)
              (switch-symlinks %guix-home generation)
              (setenv "GUIX_NEW_HOME" he-out-path)
              (primitive-load (string-append he-out-path "/activate"))
              (setenv "GUIX_NEW_HOME" #f)
              (return #f)))))))

(define* (make-home #:optional rest)
  (if (equal? rest (list "--allow-downgrades"))
      (reconfigure-home
       (eval-string
        (with-blocks '(nonguix channels machine config)
                     "(rde-config-home-environment %config)")))
      (apply (@ (guix scripts home) guix-home)
             (cons* "reconfigure"
                    (string-append
                     "--expression="
                     (with-blocks '(channels machine nonguix config)
                                  "(rde-config-home-environment %config)"))
                    "--keep-failed" "--fallback" rest))))


;;; "make all"
(define* (make-all #:optional rest)
  (eval-string
   (with-blocks '(channels config-channels) "(make-channels %channels)"))
  (make-pull rest)
  (let* ((config
          (eval-string
           (with-blocks '(channels machine nonguix config) "%config"))))
    (display "Reconfiguring system...\n")
    (reconfigure-system (rde-config-operating-system config))
    (display "Reconfiguring home...\n")
    (reconfigure-home (rde-config-home-environment config))))

;;; Dispatcher
(match-let
    ((("./make" str rest ...) (command-line)))
  (match str
    ("repl" (apply (@(guix scripts repl) guix-repl) '("-i")))
    ("channels" (eval-string
                 (with-blocks '(channels config-channels) "(make-channels %channels)")))
    (_ (eval-string
        (string-append "(make-" str
                       " (list \"" (string-join
                                    (cons* "--allow-downgrades" rest)
                                    "\" \"")
                       "\"))")))))

;; Local Variables:
;; compile-command: "GUILE_LOAD_PATH=${HOME}/spheres/info/rde/src:${GUILE_LOAD_PATH} guild compile make"
;; mode: scheme
;; End:
