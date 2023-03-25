#!/usr/bin/env -S GUILE_LOAD_PATH=${HOME}/.config/guix/current/share/guile/site/3.0/:${GUILE_LOAD_PATH}:. GUILE_LOAD_COMPILED_PATH=${HOME}/.config/guix/current/lib/guile/3.0/site-ccache/:${GUILE_LOAD_COMPILED_PATH} guix repl --
!#
;; -*- mode: scheme -*-
(use-modules
 ;; Guile libraries.
 (ice-9 match)
 (ice-9 popen)
 (ice-9 pretty-print)
 (ice-9 rdelim)
 (ice-9 string-fun)
 (srfi srfi-9 gnu)
 (srfi srfi-71)
 (srfi srfi-1)
 (git)

 (guix packages)
 (guix profiles)
 (guix base16)
 (guix gexp)

 (gnu packages)
 (gnu services)
 (gnu services base)
 (gnu system)
 (gnu system file-systems)
 (gnu system mapped-devices))


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
  (display rest)
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

;; TODO Move this into function make-system (doesn't work yet)
(include "config") ;; For %os to be defined.

(define (make-system)
  (configure-reproducible-print)
  (make-system-1 %os))

(define (make-system-1 os)
  "Call function `make-force-system-sudo' if the hash of the OS configuration has changed."
  (let* ((file "/home/graves/.config/guix/system.cache")
         (input-port (open-input-file file))
         (prev (read-line input-port))
         (new (let ((out get-hash ((@(gcrypt hash) open-sha256-port))))
                (display os out)
                (close-port out)
                (bytevector->base16-string (get-hash)))))
    (close-port input-port)
    (if (not (equal? prev new))
        (with-output-to-file file
          (lambda _
            (make-force-system-sudo)
            (display new (current-output-port)))))))

(define* (make-force-system-sudo #:optional rest)
  (apply system*
         (cons* "sudo" "-E" "RDE_TARGET=system"
                "guix" "system" "reconfigure" "./config" rest)))

(define* (make-vm #:optional rest)
  (apply system*
         (cons* "sudo" "-E" "RDE_TARGET=live-system"
                "guix" "system" "vm" "./config" rest)))


;;; Home scripts.
;; TODO make home-init target in case of from scratch installation
(define* (make-home #:optional rest)
  (apply system*
    (cons* "env" "RDE_TARGET=home"
          "guix" "home" "reconfigure" "./config"
          "--keep-failed" "--fallback" rest)))


;; Tests
(define (make-test)
  (primitive-load "config"))


;;; "make all"
(define* (make-all #:optional rest)
  (make-channels)
  (make-pull rest)
  (make-force-system-sudo rest)
  (make-home rest))

;;; Dispatcher
(match-let
    ((("./make" str rest ...) (command-line)))
  (eval-string
   (string-append "(make-" str
                  " (list \"" (string-join
                               (cons* "--allow-downgrades" rest)
                               "\" \"")
                  "\"))")))
