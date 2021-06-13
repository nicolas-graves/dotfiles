(define-module (services)
  #:use-module (ice-9 match)

  #:use-module (srfi srfi-28)

  #:use-module (guix gexp)
  #:use-module (guix records)

  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services dbus)

  #:use-module (gnu packages networking)
  #:use-module (gnu packages connman)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages linux)

  #:use-module (packages)

  #:export (connman-configuration
            pipewire-configuration
            xdg-desktop-portal-configuration
            doas-rule
            doas-configuration))


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
  (connman connman-configuration-connman
           (default connman-with-iwd))
  (disable-vpn? connman-configuration-disable-vpn?
                (default #t))
  (wifi-agent connman-configuration-wifi-provider
              (default 'iwd)))

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

(define-record-type* <pipewire-configuration>
  pipewire-configuration make-pipewire-configuration
  pipewire-configuration?
  (pipewire pipewire-configuration-pipewire
            (default pipewire-0.3)))

(define-record-type* <xdg-desktop-portal-configuration>
  xdg-desktop-portal-configuration make-xdg-desktop-portal-configuration
  xdg-desktop-portal-configuration?
  (xdg-desktop-portal xdg-desktop-portal-configuration-xdg-desktop-portal
                      (default xdg-desktop-portal)))

(define pipewire-shepherd-service
  (match-lambda
    (($ <pipewire-configuration> pipewire)
     (list (shepherd-service
            (documentation "Run Pipewire")
            (provision '(pipewire))
            (requirement '(user-processes dbus-system))
            (start #~(make-forkexec-constructor
                      (list (string-append #$pipewire "/bin/pipewire"))))
            (stop #~(make-kill-destructor)))))))

(define-public pipewire-service-type
  (let ((pipewire-package (compose list pipewire-configuration-pipewire)))
    (service-type (name 'pipewire)
                  (extensions
                   (list (service-extension shepherd-root-service-type
                                            pipewire-shepherd-service)
                         (service-extension dbus-root-service-type
                                            pipewire-package)
                         (service-extension profile-service-type
                                            pipewire-package)))
                  (default-value (pipewire-configuration))
                  (description "run pipewire"))))

(define xdp-shepherd-service
  (match-lambda
    (($ <xdg-desktop-portal-configuration> xdg-desktop-portal)
     (list (shepherd-service
            (documentation "Run xdg-desktop-portal")
            (provision '(xdg-desktop-portal))
            (requirement '(user-processes dbus-system))
            (start #~(make-forkexec-constructor
                      (list (string-append
                             #$xdg-desktop-portal
                             "/libexec/xdg-desktop-portal")
                            "-r")))
            (stop #~(make-kill-destructor)))))))

(define-public xdg-desktop-portal-service-type
  (let ((xdp-package (compose list xdg-desktop-portal-configuration-xdg-desktop-portal)))
    (service-type (name 'xdg-desktop-portal)
                  (extensions
                   (list (service-extension shepherd-root-service-type
                                            xdp-shepherd-service)
                         (service-extension dbus-root-service-type
                                            xdp-package)
                         (service-extension profile-service-type
                                            xdp-package)))
                  (default-value (xdg-desktop-portal-configuration))
                  (description "run xdg-desktop-portal"))))

(define-record-type* <doas-configuration>
  doas-configuration make-doas-configuration
  doas-configuration?
  (doas doas-configuration-doas (default doas))
  (rules doas-configuration-rules
         (default '("permit :wheel"))))

;; permit persist setenv { PKG_CACHE PKG_PATH } aja cmd pkg_add
;; permit setenv { -ENV PS1=$DOAS_PS1 SSH_AUTH_SOCK } :wheel
;; permit nopass tedu as root cmd /usr/sbin/procmap
;; permit nopass keepenv setenv { PATH } root as root
