(define-module (services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services dbus)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages connman)
  #:use-module (ice-9 match)
  #:export (connman-configuration))

(define (iwd-shepherd-service _)
  "Return a shepherd service for iwd"
  (list (shepherd-service
         (documentation "Run iwd")
         (provision '(iwd))
         (requirement
          `(user-processes dbus-system loopback))
         (start #~(make-forkexec-constructor
                   (list (string-append #$iwd "/libexec/iwd"))))
         (stop #~(make-kill-destructor)))))

(define-public iwd-service-type
  (let ((iwd-package (const (list iwd))))
    (service-type (name 'iwd)
                  (extensions
                   (list (service-extension shepherd-root-service-type
                                            iwd-shepherd-service)
                         (service-extension dbus-root-service-type
                                            iwd-package)
                         (service-extension profile-service-type
                                            iwd-package)))
                  (default-value '())
                  (description
                   "Run @url{https://01.org/iwd,iwd},
a wpa-supplicant replacemennt."))))

(define-record-type* <connman-configuration>
  connman-configuration make-connman-configuration
  connman-configuration?
  (connman      connman-configuration-connman
                (default connman))
  (disable-vpn? connman-configuration-disable-vpn?
                (default #f))
  (wifi-agent   connman-configuration-wifi-provider
                (default 'wpa-supplicant)))

(define (connman-activation config)
  (let ((disable-vpn? (connman-configuration-disable-vpn? config)))
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (mkdir-p "/var/lib/connman/")
          (unless #$disable-vpn?
            (mkdir-p "/var/lib/connman-vpn/"))))))

(define connman-shepherd-service
  (match-lambda
    (($ <connman-configuration> connman disable-vpn? wifi-agent)
     (list (shepherd-service
            (documentation "Run Connman")
            (provision '(networking))
            (requirement
             `(user-processes dbus-system loopback ,wifi-agent))
            (start #~(make-forkexec-constructor
                      (list (string-append #$connman "/sbin/connmand")
                            "-n" "-r"
                            #$@(if disable-vpn? '("--noplugin=vpn") '())
                            #$@(if (eq? 'iwd wifi-agent)
                                   '("--wifi=iwd_agent")
                                   '()))
                      #:log-file "/var/log/connman.log"))
            (stop #~(make-kill-destructor)))))))

(define-public connman-service-type
  (let ((connman-package (compose list connman-configuration-connman)))
    (service-type (name 'connman)
                  (extensions
                   (list (service-extension shepherd-root-service-type
                                            connman-shepherd-service)
                         (service-extension polkit-service-type
                                            connman-package)
                         (service-extension dbus-root-service-type
                                            connman-package)
                         (service-extension activation-service-type
                                            connman-activation)
                         (service-extension profile-service-type
                                            connman-package)))
                  (default-value (connman-configuration))
                  (description
                   "Run @url{https://01.org/connman,Connman},
a network connection manager."))))
