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
