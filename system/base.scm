(define-module (system base)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (guix gexp)

  #:use-module (gnu system)
  #:use-module (gnu system nss)

  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)

  #:use-module (gnu services)
  #:use-module (gnu services base)

  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages fonts))


(define-public packages
  (let* ((spec->pkg (compose list specification->package+output))
         (specs '("nvi" "nano" "zile" "wireless-tools"))
         (unused-pkgs (map specification->package specs)))
    (append
     (map spec->pkg '("nss-certs" "openssh" "htop"))
     (lset-difference equal? %base-packages unused-pkgs))))


(define-public services
  (modify-services %base-services
    (console-font-service-type
     config =>
     (map (cut cons <> #~(string-append #$font-terminus
                                        "/share/consolefonts/ter-132n"))
          '("tty1" "tty2" "tty3" "tty4" "tty5" "tty6")))))


(define-public system
  (operating-system
    (host-name "base")
    (timezone "Europe/Moscow")
    (locale-libcs (list (canonical-package glibc)))
    (file-systems '())
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (target "/boot")))
    (name-service-switch %mdns-host-lookup-nss)))
