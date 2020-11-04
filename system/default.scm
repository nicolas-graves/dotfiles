(define-module (default)
  #:use-module (guile)
  #:use-module (gnu)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages zile)
  #:use-module (gnu packages nano)
  #:use-module (gnu packages version-control)
  #:export (base-system base-packages))

(define base-packages
  (let ([unused? (λ (p) (not (member p `(,nano
                                         ,zile
                                         ,wireless-tools
                                         ))))])
    (cons*
     nss-certs
     git
     (filter unused? %base-packages))))

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
