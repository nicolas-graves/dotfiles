(define-module (system desktop)
  #:use-module (gnu)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)

  #:use-module (gnu system)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system pam)

  #:use-module (gnu packages)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages display-managers)
  #:use-module (gnu packages fonts)

  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services dbus)
  #:use-module (gnu services networking)
  #:use-module (gnu services linux)
  #:use-module (gnu services xorg)
  #:use-module (gnu services cups)
  #:use-module (gnu services pm)

  #:use-module ((system base) :prefix base:)
  #:use-module (services)
  #:export (pam-services))


;; Allow members of the "video" group to change the screen brightness.
(define %backlight-udev-rule
  (udev-rule
   "90-backlight.rules"
   (string-append "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\""
                  "\n"
                  "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))


(define %my-desktop-services
  (let* ((path "/share/consolefonts/ter-132n")
         (font #~(string-append #$font-terminus #$path))
         (ttys '("tty1" "tty2" "tty3" "tty4" "tty5" "tty6")))
    (modify-services %desktop-services
                     (elogind-service-type config =>
                                           (elogind-configuration (inherit config)
                                                                  (handle-lid-switch-external-power 'suspend)))
                     (udev-service-type config =>
                                        (udev-configuration (inherit config)
                                                            (rules (cons %backlight-udev-rule
                                                                         (udev-configuration-rules config)))))
		     (console-font-service-type config =>
						(map (cut cons <> font) ttys))
		     )))


(define* pam-services

  (append (base-pam-services)

          (list (pam-service
                 (name "pam_gnupg")
                 (session
                  (list (pam-entry
                         (control "required")
                         (module (file-append pam-gnupg "lib/security/pam_gnupg.so")))))))
          ))

(define-public services
    (cons*
      (pam-limits-service ;; This enables JACK to enter realtime mode
	(list
	  (pam-limits-entry "@realtime" 'both 'rtprio 99)
	  (pam-limits-entry "@realtime" 'both 'memlock 'unlimited)))
      (extra-special-file "/usr/bin/env"
			  (file-append coreutils "/bin/env"))
      (service thermald-service-type)
      (service bluetooth-service-type
               (bluetooth-configuration
                (auto-enable? #t)))
      (service cups-service-type
               (cups-configuration
                (extensions (list splix cups-filters))
                (default-paper-size "A4")
                (web-interface? #t)))
      (remove (lambda (service)
		(eq? (service-kind service) gdm-service-type))
	      %my-desktop-services)))

(define-public packages
  (append
   base:packages
   (map specification->package
        '("ntfs-3g"
          ;; "sway@1.5.1"
          "qtwayland"
          "intel-vaapi-driver"
          "libva-utils"))))


(define-public groups
  (cons (user-group (system? #t) (name "realtime"))
	%base-groups))


(define-public system base:system)
