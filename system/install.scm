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
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system)
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
  #:export (installation-os-nonfree))

(define packages
  (append
   desktop:packages
   (map specification->package
        '("curl"
          "htop"
          "emacs-no-x-toolkit"
          "network-manager"
          "swaylock"))))

(define yggdrasil-services
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
    (inherit desktop:system)
    (kernel linux)
    (firmware (list linux-firmware))
    (services yggdrasil-services)

    ;; Add the 'net.ifnames' argument to prevent network interfaces
    ;; from having really long names.  This can cause an issue with
    ;; wpa_supplicant when you try to connect to a wifi network.
    (kernel-arguments '("quiet" "modprobe.blacklist=radeon" "net.ifnames=0"))

    ;; Add some extra packages useful for the installation process
    (packages packages)))

installation-os-nonfree
