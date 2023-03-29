#!/usr/bin/env -S GUILE_LOAD_PATH=${HOME}/.config/guix/current/share/guile/site/3.0/:${GUILE_LOAD_PATH}:. GUILE_LOAD_COMPILED_PATH=${HOME}/.config/guix/current/lib/guile/3.0/site-ccache/:${GUILE_LOAD_COMPILED_PATH} guix repl --
!#
;; -*- mode: scheme -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2022,2023 Nicolas Graves <ngraves@ngraves.fr>

;; Modules for config.
(use-modules

 (ice-9 match)
 (ice-9 popen)
 (ice-9 pretty-print)
 (ice-9 rdelim)

 (srfi srfi-1)

 (guix download)
 (guix gexp)
 (guix packages)

 ((guix build utils) #:select (find-files))
 ((gnu packages) #:select (specification->package))
 ((rde packages) #:select (strings->packages))
 ((gnu services) #:select (simple-service etc-service-type service))

 (gnu system)
 (gnu system file-systems)
 (gnu system mapped-devices)
 (gnu home services xdg)

 (rde features)
 (rde features base)
 (rde features emacs-xyz)
 (rde features fontutils)
 (rde features keyboard)
 (rde features linux)
 (rde features markup)
 (rde features networking)
 (rde features shells)
 (rde features shellutils)
 (rde features system)
 (rde features terminals)
 (rde features video)
 (rde features web-browsers)
 (contrib features emacs-xyz)
 (nongnu packages linux)

 (features))

;; Additional modules for make.
(use-modules
 ;; Guile libraries.
 (srfi srfi-9 gnu)
 (srfi srfi-71)
 (git)

 (guix channels)
 (guix profiles)
 (guix base16)
 (guix store)
 (guix ui)
 (guix monads)
 (guix derivations)

 (gnu system)
 (gnu system image)
 (gnu image)
 )


;;; Nonguix features.

(begin
  (use-modules (guix records))

  (define-record-type* <machine> machine make-machine
    machine?
    this-machine
    (name machine-name) ;string
    (efi machine-efi) ;file-system
    (swap machine-swap) ;file-system
    (uuid-mapped machine-uuid-mapped) ;uuid
    (firmware machine-firmware (default '())) ;list of packages
    (features machine-features (default '()))) ;list of features

  (define (get-hardware-features machines)

    (define (get-mapped-device local-machine)
      (mapped-device
       (source (uuid (machine-uuid-mapped local-machine)))
       (targets (list "enc"))
       (type luks-device-mapping)))

    (define (get-btrfs-file-system local-machine)
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
             (dependencies (list (get-mapped-device local-machine))))))
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
               (device (machine-efi local-machine))))))

    (define (get-machine-name)
      "This function looks up the hardcoded value of the current machine name."
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
         #:swap-devices (list (swap-space (target (machine-swap machine))))
         #:file-systems (get-btrfs-file-system machine))
        (feature-kernel
         #:kernel linux
         #:initrd microcode-initrd
         #:initrd-modules
         (append (list "vmd") (@(gnu system linux-initrd) %base-initrd-modules))
         #:kernel-arguments
         (append (list "quiet" "rootfstype=btrfs") %default-kernel-arguments)
         #:firmware (machine-firmware machine)))))))


;;; Nonguix features.

(begin
  (use-modules (nongnu system linux-initrd))

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
             (@ (gnu services base) %default-authorized-guix-keys)))))



;;; Live systems.
(begin
  (use-modules (gnu services networking)
               (rde system install))
  (define (live-install user-preferences)
    (live-os
     #:kernel linux
     #:kernel-firmware (list linux-firmware)
     #:guix-substitute-urls (list "https://substitutes.nonguix.org")
     #:guix-authorized-keys (list nonguix-key)
     #:supplementary-system-packages
     (strings->packages "vim" "git" "zip" "unzip" "make" "curl"
                        "exfat-utils" "fuse-exfat" "ntfs-3g")
     #:custom-system-services
     (list
      (simple-service
       'channels-and-sources
       etc-service-type
       `(("channels.scm" ,(local-file "/home/graves/.config/guix/channels.scm"))
         ("guix-sources" ,(local-file "/home/graves/spheres/info/guix" #:recursive? #t))
         ("nonguix-sources" ,(local-file "/home/graves/spheres/info/nonguix" #:recursive? #t))
         ("rde-sources" ,(local-file "/home/graves/spheres/info/rde" #:recursive? #t))))
      (service wpa-supplicant-service-type)
      (service network-manager-service-type))
     #:supplementary-features
     (append user-preferences (list (feature-hidpi))))))

(define* (make-live-install #:optional rest)
  (apply
   (@ (guix scripts system) guix-system)
   (cons* "image"
          (string-append
           "--expression="
           (with-hardware
            (with-nonguix
             (with-config
              (string-append
               "(include \"" (tangle-make-sexp 5) "\") " ;5th block is live-install.
               "(live-install %user-preferences)")))))
          "--image-size=14G"
          rest)))


;;; Utils

(define (make-sexp nth)
  "Extract NTH sexpression from make (this file)."
  (let* ((port (open-input-file "/home/graves/spheres/info/dots/make"))
         (value (last (map (lambda (k) (read port)) (iota nth 1 1)))))
    (close-port port)
    (object->string value)))

(define (tangle-make-sexp nth)
  "Return a temporary filename with the NTH sexpression from make (this file)."
  (let* ((port (mkstemp "/tmp/make-sexp-XXXXXX"))
         (file (port-filename port)))
    (format port "~a" (make-sexp nth))
    (close-port port)
    file))

(define (with-config str)
  (string-append
   "(begin (include \"" (tangle-make-sexp 1) "\")" ;1st block is common modules.
   " (include \"/home/graves/spheres/info/dots/config\") "
   str ")"))

(define (with-nonguix str)
  (string-append
   "(begin (include \"" (tangle-make-sexp 4) "\") " ;4th block is nonguix.
   str ")"))

(define (with-hardware str)
  (string-append
   "(begin (include \"" (tangle-make-sexp 3) "\") " ;3rd block is hardware.
   str ")"))


;;; Channels scripts
(define* (channel-content
          #:key
          (freeze? #f)
          (urls
           '((nonguix . "https://gitlab.com/nonguix/nonguix.git")
             (rde     . "https://git.sr.ht/~abcdw/rde")
             (guix    . "https://git.savannah.gnu.org/git/guix.git")))
          (freeze-commits
           '((nonguix . "1aecd24155019cc524bca1c868729102c8b23f24")
             (rde     . "101313a691f074dcb34e9cbd4f13664df02f4ac7")
             (guix    . "688c3ef28220979e79ffd061c762bda84a663534"))))
  "This function generates then content of the channels file, with
optional commit pinning."
  `(list
    (channel
     (name 'nonguix)
     (url ,(cdr (assoc 'nonguix urls)))
     ,(if freeze? `(commit ,(cdr (assoc 'nonguix freeze-commits)))
          `(branch "master"))
     (introduction
      (make-channel-introduction
       "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
       (openpgp-fingerprint
        "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
    (channel
     (name 'rde)
     (url ,(cdr (assoc 'rde urls)))
     ,(if freeze? `(commit ,(cdr (assoc 'rde freeze-commits)))
          `(branch "master"))
     (introduction
      (make-channel-introduction
       "257cebd587b66e4d865b3537a9a88cccd7107c95"
       (openpgp-fingerprint
        "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
    (channel
     (name 'guix)
     (url ,(cdr (assoc 'guix urls)))
     ,(if freeze? `(commit ,(cdr (assoc 'guix freeze-commits)))
          `(branch "master"))
     (introduction
      (make-channel-introduction
       "9edb3f66fd807b096b48283debdcddccfea34bad"
       (openpgp-fingerprint
        "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))

(define (make-channels)
  (with-output-to-file
      (string-append (getenv "HOME") "/.config/guix/channels.scm")
    (lambda ()
      (pretty-print
       (channel-content
        #:freeze? #f
        #:freeze-commits
        '((guix    . "5f8c11d48e4949aa77d7aaa1e7e25568bd8dfa97")
          (nonguix . "e026dba1dad924aa09da8a28caa343a8ace3f6c7")
          (rde     . "74a3fb8378e86603bb0f70b260cbf46286693392"))
        #:urls
        '((guix    . "/home/graves/spheres/info/guix")
          (nonguix . "/home/graves/spheres/info/nonguix")
          (rde     . "/home/graves/spheres/info/rde")))))))


;;; Pull scripts
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
             (profile-manifest
              (string-append (getenv "HOME") "/.config/guix/current")))))
   (display "Nothing to be done")
   (make-force-pull rest)))

(define* (make-force-pull #:optional rest)
  (apply (@ (guix scripts pull) guix-pull)
         (cons* "--disable-authentication" "--allow-downgrades"
                (string-append "--channels=" (getenv "HOME") "/.config/guix/channels.scm")
                (string-append "--profile="  (getenv "HOME") "/.config/guix/current")
                rest)))


;;; System scripts
(define (configure-reproducible-print)
  "Modify guix record printers to achieve reproducible system description
output. Currently not perfect, mostly because of thunked procedures which use
object adresses."
  (set-record-type-printer! (@@ (guix packages) <package>)
                            (lambda (package port)
                              (let ((format simple-format))
                                (format port "#<package ~a@~a ~a>"
                                        (package-name package)
                                        (package-version package)
                                        (package-source package)))))


  (define (print-origin origin port)
    "Write a concise representation of ORIGIN to PORT."
    (match origin
      (($ (@@ (guix packages) <origin>) uri method hash file-name patches)
       (simple-format port "#<origin ~s ~a ~s>"
                      uri hash
                      (force patches)))))

  (set-record-type-printer! (@@ (guix packages) <origin>) print-origin)

  (define (write-gexp gexp port)
    "Write GEXP on PORT."
    (display "#<gexp " port)

    ;; Try to write the underlying sexp.  Now, this trick doesn't work when
    ;; doing things like (ungexp-splicing (gexp ())) because GEXP's procedure
    ;; tries to use 'append' on that, which fails with wrong-type-arg.
    (false-if-exception
     (write (apply (gexp-proc gexp)
                   (gexp-references gexp))
            port))

    (let ((loc ((@@ (guix gexp) gexp-location) gexp)))
      (when loc
        (format port " ~a" ((@@ (guix diagnostics) location->string) loc)))))

  (set-record-type-printer! (@@ (guix gexp) <gexp>) write-gexp)

  (set-record-type-printer! (@@ (gnu system) <operating-system>)
                            (lambda (operating-system port)
                              (let ((format simple-format))
                                (format port
                                        (string-append
                                         "#<<operating-system> "
                                         (string-join (make-list 25 "~a") " ")
                                         ">")
                                        (operating-system-kernel operating-system)
                                        (operating-system-kernel-loadable-modules operating-system)
                                        ;; (operating-system-kernel-arguments operating-system)
                                        (operating-system-hurd operating-system)
                                        (operating-system-bootloader operating-system)
                                        ;; (operating-system-label operating-system)
                                        (operating-system-keyboard-layout operating-system)
                                        (operating-system-initrd operating-system)
                                        (operating-system-initrd-modules operating-system)
                                        (operating-system-firmware operating-system)
                                        (operating-system-host-name operating-system)
                                        ;; (operating-system-hosts-file operating-system)
                                        (operating-system-mapped-devices operating-system)
                                        (operating-system-file-systems operating-system)
                                        ;; (operating-system-swap-devices operating-system)
                                        (operating-system-users operating-system)
                                        (operating-system-groups operating-system)
                                        (operating-system-skeletons operating-system)
                                        (operating-system-issue operating-system)
                                        (operating-system-packages operating-system)
                                        (operating-system-timezone operating-system)
                                        (operating-system-locale operating-system)
                                        (operating-system-locale-definitions operating-system)
                                        (operating-system-locale-libcs operating-system)
                                        (operating-system-name-service-switch operating-system)
                                        ;; (operating-system-essential-services operating-system)
                                        (operating-system-user-services operating-system)
                                        (operating-system-pam-services operating-system)
                                        (operating-system-setuid-programs operating-system)
                                        (operating-system-sudoers-file operating-system)
                                        ;; (operating-system-location operating-system)
                                        ))))

  (define (write-service-type type port)
    (simple-format port "#<service-type ~a ~a>"
                   (service-type-name type)
                   (service-type-extensions type) ;; This alone makes the file huge.
                   ;; (service-type-compose type)
                   ;; (service-type-extend type)
                   ))

  (define (write-service-type-extension type-extension port)
    (simple-format port "#<service-extension ~a>"
                   (service-extension-target type-extension)
                   ;; (service-extension-compute type-extension)
                   ))

  (set-record-type-printer! (@@ (gnu services) <service-type>) write-service-type)
  (set-record-type-printer! (@@ (gnu services) <service-extension>) write-service-type-extension)

  (set-record-type-printer! (@@ (gnu services base) <greetd-terminal-configuration>)
                            (lambda (greetd port)
                              (format port "#<<greetd-terminal-configuration> ~a ~a ~a ~a ~a ~a>"
                                      ((@ (gnu services base) greetd-package) greetd)
                                      ;; ((@ (gnu services base) greet-config-file-name) greetd)
                                      ;; ((@ (gnu services base) greet-log-file-name) greetd)
                                      ((@ (gnu services base) greetd-terminal-vt) greetd)
                                      ((@ (gnu services base) greetd-terminal-switch) greetd)
                                      ((@ (gnu services base) greetd-source-profile?) greetd)
                                      ((@ (gnu services base) greetd-default-session-user) greetd)
                                      ((@ (gnu services base) greetd-default-session-command) greetd))))

  (set-record-type-printer! (@@ (gnu services base) <network-address>)
                            (lambda (network-address port)
                              (simple-format port "#<<network-address> ~a ~a>"
                                             (network-address-device network-address)
                                             (network-address-value network-address)
                                             ;; (network-address-ipv6? network-address)
                                             )))

  (set-record-type-printer! (@@ (guix gexp) <system-binding>)
                            (lambda (system-binding port)
                              (format port "#<<system-binding>>"
                                      ;; ((@@ (guix gexp) system-binding-proc) system-binding)
                                      )))

  (set-record-type-printer! (@@ (gnu system mapped-devices) <mapped-device>)
                            (lambda (mapped-device port)
                              (simple-format port "#<<mapped-device> ~a ~a ~a>"
                                             (mapped-device-source mapped-device)
                                             (mapped-device-targets mapped-device)
                                             (mapped-device-type mapped-device)
                                             ;; (mapped-device-location mapped-device)
                                             )))

  (set-record-type-printer! (@@ (gnu system file-systems) <file-system>)
                            (lambda (file-system port)
                              (simple-format port
                                             (string-append
                                              "#<<file-system> "
                                              (string-join (make-list 12 "~a") " ") ">")
                                             (file-system-device file-system)
                                             (file-system-mount-point file-system)
                                             (file-system-type file-system)
                                             (file-system-flags file-system)
                                             (file-system-options file-system)
                                             (file-system-mount? file-system)
                                             (file-system-mount-may-fail? file-system)
                                             (file-system-needed-for-boot? file-system)
                                             (file-system-check? file-system)
                                             (file-system-skip-check-if-clean? file-system)
                                             (file-system-repair file-system)
                                             (file-system-create-mount-point? file-system)
                                             ;; (file-system-dependencies file-system)
                                             ;; (file-system-location file-system)
                                             ))))

;; System should not rely on printers, but on the derivation produced.
;; But seems complicated to get the derivation file within guile.

;; (define (make-system)
;;   (configure-reproducible-print)
;;   (make-system-1 %os))

;; (define (make-system-1 os)
;;   "Call function `make-force-system-sudo' if the derivation of the OS configuration has changed."
;;   (let* ((file "/home/graves/.config/guix/system.cache")
;;          (input-port (open-input-file file))
;;          (prev (read-line input-port))
;;          (new (let ((out get-hash ((@(gcrypt hash) open-sha256-port))))
;;                 (display (lower-object os) out)
;;                 (close-port out)
;;                 (bytevector->base16-string (get-hash)))))
;;     (close-port input-port)
;;     (if (not (equal? prev new))
;;         (with-output-to-file file
;;           (lambda _
;;             (make-force-system-sudo)
;;             (display new (current-output-port)))))))

(define* (make-force-system-sudo #:optional rest)
  (apply system*
         (cons* "sudo" "-E" "guix" "system" "reconfigure"
                (string-append
                 "--expression="
                 (with-hardware
                  (with-nonguix
                   (with-config "(rde-config-operating-system %config)"))))
                rest)))


;;; Home scripts.
;; TODO make home-init target in case of from scratch installation
(define* (make-home #:optional rest)
  (apply (@ (guix scripts home) guix-home)
         (cons* "reconfigure"
                (string-append
                 "--expression="
                 (with-hardware
                  (with-nonguix
                   (with-config "(rde-config-home-environment %config)"))))
                "--keep-failed" "--fallback" rest)))


;; Print
;; (define (make-print rest)
;;   (eval-string
;;    (with-config
;;     (match rest
;;       ("home" "(display (rde-config-home-environment %config))")
;;       ("system" "(rde-config-operating-system %config)")
;;       (_ "(rde-config-home-environment %config)")))))


;;; "make all"
(define* (make-all #:optional rest)
  (make-channels)
  (make-pull rest)
  (make-force-system-sudo rest)
  (make-home rest))

;;; Dispatcher
(match-let
    ((("./make" str rest ...) (command-line)))
  (match str
    ("repl" (apply (@(guix scripts repl) guix-repl) '("-i")))
    ("channels" (make-channels))
    (_ (eval-string
        (string-append "(make-" str
                       " (list \"" (string-join
                                    (cons* "--allow-downgrades" rest)
                                    "\" \"")
                       "\"))")))))
