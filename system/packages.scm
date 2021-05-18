(define-module (packages)
  #:use-module (gnu packages)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module (guix transformations)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages image)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages connman)
  #:use-module ((guix licenses) #:prefix license:))


(define-public connman-with-iwd
  (package/inherit connman
    (arguments
     (substitute-keyword-arguments (package-arguments connman)
       ((#:configure-flags flags)
        `(cons "--enable-iwd" ,flags))))))


(define-public xdg-desktop-portal-wlr
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
