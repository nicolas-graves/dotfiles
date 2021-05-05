(define-module (default)
  #:use-module (guile)
  #:use-module (gnu)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages zile)
  #:use-module (gnu packages nano)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages fonts)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (base-system base-packages base-services))


(define base-packages
  (let ((unused-pkgs (list nano zile wireless-tools)))
    (cons*
     nss-certs
     git
     (lset-difference equal? %base-packages unused-pkgs))))


(define base-services
  (modify-services %base-services
    (console-font-service-type config =>
                               (map (cut cons <> #~(string-append #$font-terminus "/share/consolefonts/ter-132n"))
                                    '("tty1" "tty2" "tty3" "tty4" "tty5" "tty6")))))


(define base-system
  (operating-system
    (kernel linux-libre)
    (host-name "default")
    (timezone "Europe/Moscow")
    (locale-libcs (list glibc-2.29 (canonical-package glibc)))
    (file-systems #f)
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (target "/boot/efi")))
    (name-service-switch %mdns-host-lookup-nss)))
