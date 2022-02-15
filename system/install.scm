;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 David Wilson <david@daviwil.com>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Generate a bootable image (e.g. for USB sticks, etc.) with:
;; $ guix system disk-image nongnu/system/install.scm

(define-module (nongnu system install)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system install)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages file-systems)
  #:use-module (nongnu packages linux)
  #:use-module ((system desktop) #:prefix desktop:)
  #:use-module ((system connections) #:prefix connections:)
  #:use-module (home yggdrasil rbw)
  #:export (installation-os-nonfree))

(define %backing-directory
  ;; Sub-directory used as the backing store for copy-on-write.
  "/tmp/guix-inst")

(define cow-store-service-type
  (shepherd-service-type
   'cow-store
   (lambda _
     (define (import-module? module)
       ;; Since we don't use deduplication support in 'populate-store', don't
       ;; import (guix store deduplication) and its dependencies, which
       ;; includes Guile-Gcrypt.
       (and (guix-module-name? module)
            (not (equal? module '(guix store deduplication)))))

     (shepherd-service
      (requirement '(root-file-system user-processes))
      (provision '(cow-store))
      (documentation
       "Make the store copy-on-write, with writes going to \
the given target.")

      ;; This is meant to be explicitly started by the user.
      (auto-start? #f)

      (modules `((gnu build install)
                 ,@%default-modules))
      (start
       (with-imported-modules (source-module-closure
                               '((gnu build install))
                               #:select? import-module?)
         #~(case-lambda
             ((target)
              (mount-cow-store target #$%backing-directory)
              target)
             (else
              ;; Do nothing, and mark the service as stopped.
              #f))))
      (stop #~(lambda (target)
                ;; Delete the temporary directory, but leave everything
                ;; mounted as there may still be processes using it since
                ;; 'user-processes' doesn't depend on us.  The 'user-file-systems'
                ;; service will unmount TARGET eventually.
                (delete-file-recursively
                 (string-append target #$%backing-directory))))))
   (description "Make the store copy-on-write, with writes going to \
the given target.")))

(define (cow-store-service)
  "Return a service that makes the store copy-on-write, such that writes go to
the user's target storage device rather than on the RAM disk."
  ;; See <http://bugs.gnu.org/18061> for the initial report.
  (service cow-store-service-type 'mooooh!))

(define packages
  (append
   desktop:packages
   (map specification->package
        '("curl"
          "htop"
          "emacs-no-x-toolkit"
          "network-manager"
          "swaylock"
          "glibc"
          "fontconfig"
          "font-dejavu"
          "font-gnu-unifont"
          "grub"
          "pinentry"
          "rbw@1.4.3"
          "nss-certs"))))

(define services
   (cons*
    ;; (service tlp-service-type
    ;;          (tlp-configuration
    ;;           (cpu-boost-on-ac? #t)
    ;;           (wifi-pwr-on-bat? #t)))
    ;; (service
    ;;  screen-locker-service-type
    ;;  (screen-locker "swaylock"
    ;;                 (file-append swaylock "/bin/swaylock")
    ;;                 #f))

    connections:services
    ;; Add the 'cow-store' service, which users have to start manually
    ;; since it takes the installation directory as an argument.
    (cow-store-service)

    ;; To facilitate copy/paste.
    (service gpm-service-type)

    (modify-services desktop:services
      (guix-service-type
       config => (guix-configuration
                  (inherit config)
                  (substitute-urls (cons*
                                    "https://substitutes.nonguix.org"
                                    (string-append "https://" (getenv "URI_service_substitutes"))
                                    %default-substitute-urls))
                  (authorized-keys (cons*
                                    (local-file "../keys/nonguix.pub")
                                    (local-file "../keys/my-substitutes-key.pub")
                                    %default-authorized-guix-keys)))))))

(define installation-os-nonfree
  (operating-system
    (inherit installation-os)
    (kernel linux)
    (firmware (list linux-firmware))
    (services services)

    (skeletons
     `((".config_rbw_config.json" ,rbw-config-bitwarden)
       (".config_guix_channels.scm" ,(local-file "../channels.base"))))

    ;; Add some extra packages useful for the installation process
    (packages packages)))

installation-os-nonfree
