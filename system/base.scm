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
  (let* ((spec->pkg (compose list specification->package+output)))
    (append
     (map spec->pkg '("nss-certs"
                      "vim"
                      "git"
                      "wireless-tools"
                      "exfat-utils"
                      "fuse-exfat"
                      "openssh-sans-x"
                      "openssl"
                      "zip"
                      "unzip"
                      "trash-cli"
                      "gnupg"
                      "htop"))
     %base-packages)))


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
