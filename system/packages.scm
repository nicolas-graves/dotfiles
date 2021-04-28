(define-module (packages)
  #:use-module (gnu packages)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module (guix transformations)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages glib)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (xdg-desktop-portal-wlr))


(define pipewire-aaa
  (package/inherit pipewire-0.3
    (arguments
     (substitute-keyword-arguments (package-arguments pipewire-0.3)
       ((#:configure-flags flags)
        `(cons "--prefix=/run/current-system/profile"
               ,flags))))))


(define xdg-desktop-portal-wlr
  (package
    (name "xdg-desktop-portal-wlr")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/emersion/xdg-desktop-portal-wlr")
                    (commit (string-append "v" version))))
              (sha256
               (base32 "1vjz0y3ib1xw25z8hl679l2p6g4zcg7b8fcd502bhmnqgwgdcsfx"))))
    (build-system meson-build-system)
    (native-inputs
     `(("wayland-protocols" ,wayland-protocols)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("pipewire" ,pipewire-0.3)
       ("wayland" ,wayland)
       ("elogind" ,elogind)
       ("libdrm" ,libdrm)))
    (arguments
     `(#:configure-flags
       '("-Dsystemd=disabled"
         "-Dsd-bus-provider=libelogind")))
    (synopsis "")
    (description "")
    (home-page "")
    (license license:expat)))
