(define-module (home packages vpn)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages check)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages base)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages man)
  #:use-module (gnu packages linux))

;; This version of the package should work well, but the services are not the
;; same. At first sight, they seem difficult to extend: everything more or
;; less relies on systemd. I'm stopping any further development for now, but
;; keep config in git history so that I can use it later if useful.
;; https://user-images.githubusercontent.com/1685255/116310665-9a4dc900-a7aa-11eb-9c18-c6dc1512fc43.png

(define openvpn3-core
  (let* ((commit "c4fa5a69c5d2e4ba4a86e79da8de0fc95f95edc3")
         (revision "0"))
    (package
     (name "openvpn3-core")
     (version (git-version "3.7" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OpenVPN/openvpn3")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08agdwx8rsq0xywm06ng4pg210i5vbpqnf085abxla29gv3j852n"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "deps")
           (delete-file "cmake/dlgoogletest.cmake")
           (substitute* "test/unittests/CMakeLists.txt"
                        ((".*dlgoogletest.*") ""))))))
     (build-system copy-build-system)
     ;; It's possible to build the library through cmake-build-system, but
     ;; tricky, and that doesn't help with openvpn3. Here are the inputs to do
     ;; so.
     ;; (native-inputs
      ;; (list swig pkg-config autoconf autoconf-archive automake python-docutils))
     ;; (inputs
      ;; (list lz4 lzo openssl glib jsoncpp libcap-ng util-linux python python-jinja2
            ;; asio xxhash libcap googletest))
     (home-page "https://openvpn.net/")
     (synopsis "Virtual private network daemon - core library")
     (description
      "OpenVPN implements virtual private network (@dfn{VPN}) techniques
for creating secure point-to-point or site-to-site connections in routed or
bridged configurations and remote access facilities.  It uses a custom
security protocol that utilizes SSL/TLS for key exchange.  It is capable of
traversing network address translators (@dfn{NAT}s) and firewalls.  This
package only provides the source library for the openvpn3 package.")
     (license license:gpl2))))

(define-public openvpn3
  (package
   (name "openvpn3")
   (version "v18_beta")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/OpenVPN/openvpn3-linux")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0m5ccz5lpl17lkakv0yg8pxl4mdpnh3b9av62c8s96y4g2f92cp1"))
     (modules '((guix build utils)))
     (snippet
      #~(begin
          (delete-file-recursively "vendor")
          (delete-file-recursively "openvpn3-core")
          (delete-file-recursively "ovpn-dco")
          (substitute* "bootstrap.sh"
                       ((".*git.*") ""))
          (substitute*
              "update-version-m4.sh"
            (("#!/bin/sh") (string-append  "#!" #$bash "/bin/sh"))
            (("^VERSION=.*") (string-append "VERSION=" "v18_beta")))
          (substitute* "configure.ac"
                       ((".*Missing openvpn3-core version information.*")
(string-append
                         "OPENVPN3_CORE_VERSION=\""
                         (package-version openvpn3-core)
                         "\"\n")))))))
   (build-system gnu-build-system)
   (arguments
    (list
     #:configure-flags
     #~(list (string-append "ASIO_SOURCEDIR=" #$asio "/include")
             (string-append
              "OPENVPN3_CORE_SOURCEDIR=" #$openvpn3-core)
             "--disable-unit-tests")))
   (native-inputs
    (list pkg-config autoconf autoconf-archive automake python-docutils m4
          libxml2))
   (inputs
    (list lz4 lzo openssl glib jsoncpp libcap-ng util-linux python
          python-jinja2 openvpn3-core tinyxml2 asio))
   (home-page "https://openvpn.net/")
   (synopsis "Virtual private network daemon")
   (description
    "OpenVPN implements virtual private network (@dfn{VPN}) techniques
for creating secure point-to-point or site-to-site connections in routed or
bridged configurations and remote access facilities.  It uses a custom
security protocol that utilizes SSL/TLS for key exchange.  It is capable of
traversing network address translators (@dfn{NAT}s) and firewalls.  This
package implements OpenVPN3, a version incompatible with OpenVPN version
numbered below 3.0. System services have not yet been written for OpenVPN3, so
the package won't work yet.")
   (license license:gpl2)))
