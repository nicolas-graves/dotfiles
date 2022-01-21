(define-module (server rsync)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (guix gexp)

  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system file-systems)

  #:use-module (gnu services)
  #:use-module (gnu services rsync)

  #:use-module (gnu packages))

(define-public packages
  '("rsync"))

(define-public services
  (list
   (service rsync-service-type
            (rsync-configuration
             (read-only? #t)))))
