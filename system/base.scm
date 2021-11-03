(define-module (system base)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (guix gexp)

  #:use-module (gnu system)
  #:use-module (gnu system nss)
  #:use-module (gnu system keyboard)

  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)

  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services security-token)

  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)

  #:use-module (services))


(define-public packages
  (let* ((spec->pkg (compose list specification->package+output))
         (specs '("vim"
                  "git"
                  "wireless-tools"
                  "exfat-utils"
                  "fuse-exfat"
                  "openssh"
                  "openssl"
                  "zip"
                  "unzip"
                  "trash-cli"
                  "gnupg"
                  ))
         (unused-pkgs (map specification->package specs)))
    (append
     (map spec->pkg '("nss-certs" "htop"))
     (lset-difference equal? %base-packages unused-pkgs))))


(define-public services
  (let* ((path "/share/consolefonts/ter-132n")
         (font #~(string-append #$font-terminus #$path))
         (ttys '("tty1" "tty2" "tty3" "tty4" "tty5" "tty6")))
    (cons*
     (service
      opendoas-service-type
      (opendoas-configuration
       (config
        `((permit :wheel)
          (permit keepenv :wheel cmd guix)))))
     (service pcscd-service-type)
     (modify-services %base-services
       (console-font-service-type
        config =>
        (map (cut cons <> font) ttys))))))


(define-public system
  (operating-system
    (host-name "base")
    (timezone "Europe/Paris")
    (locale-libcs (list (canonical-package glibc)))
    (locale "fr_FR.utf8")
    (file-systems '())
    (keyboard-layout (keyboard-layout "fr"))
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets '("/boot/efi"))
		 (keyboard-layout keyboard-layout)))
    (name-service-switch %mdns-host-lookup-nss)))
