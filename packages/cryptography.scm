(define-module (packages cryptography)
  #:use-module (gnu packages)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages xdisorg)

  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:))

(define-public go-filippo-io-edwards25519
  (package
    (name "go-filippo-io-edwards25519")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/FiloSottile/edwards25519")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01m8hpaj0cwp250f7b0din09cf8j6j5y631grx67qfhvfrmwr1zr"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "filippo.io/edwards25519"))
    (home-page "https://filippo.io/edwards25519")
    (synopsis "filippo.io/edwards25519")
    (description
     "Package edwards25519 implements group logic for the twisted Edwards curve")
    (license license:bsd-3)))

(define-public age
  (package
    (name "age")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FiloSottile/age")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19fz68n262kvg2ssw4r6nik30zk6g6cy7rdi0fm05czwigqrdz1i"))))
    (build-system go-build-system)
    (arguments `(#:import-path "filippo.io/age"))
    (propagated-inputs
     (list go-filippo-io-cmd-age
           go-filippo-io-cmd-age-keygen))
    (home-page "https://filippo.io/age")
    (synopsis "Secure file encryption tool, format, and Go library")
    (description
     "This package implements file encryption according to the
@code{age-encryption.org/v1} specification. It features small explicit keys,
no config options, and UNIX-style composability.")
    (license license:bsd-3)))

(define-public go-filippo-io-cmd-age
  (package
    (inherit age)
    (name "go-filippo-io-cmd-age")
    (propagated-inputs
     (list go-golang-org-x-sys
           go-golang-org-x-term
           go-golang-org-x-crypto
           go-filippo-io-edwards25519))
    (arguments
     `(#:import-path "filippo.io/age/cmd/age"
       #:unpack-path "filippo.io/age"
       #:install-source? #f))))

(define-public go-filippo-io-cmd-age-keygen
  (package
    (inherit go-filippo-io-cmd-age)
    (name "go-filippo-io-cmd-age-keygen")
    (arguments
     `(#:import-path "filippo.io/age/cmd/age-keygen"
       #:unpack-path "filippo.io/age"
       #:install-source? #f))))

(define-public passage
  (package
    (inherit password-store)
    (name "passage")
    (version "1.7.4a0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FiloSottile/passage")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17899whffnpqqx9x1nx2b8bfxbxlh1pwlglqa0kznl0cn6sb37ql"))))
    (build-system copy-build-system)
    (propagated-inputs
     (list util-linux
           git
           qrencode
           sed
           tree
           wl-clipboard))
    (build-system copy-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
           (add-after 'unpack 'rename-exe
             (lambda _
               (rename-file "src/password-store.sh"
                            "src/passage"))))
       #:install-plan
       (list '("src/passage" "/bin/")
             '("src/completion/pass.bash-completion"
               "/share/bash-completion/completions/")
             '("src/completion/pass.zsh-completion"
               "/share/zsh/site-functions/"))))
    (home-page "https://github.com/FiloSottile/passage")
    (synopsis "A fork of the password-store encrypted password manager")
    (description "This package provides a fork of the @code{password-store}
encrypted password manager.  It relies on @code{age} instead of
@code{gnupg}.")
    (license license:gpl2+)))

(define-public libfido2
  (package
    (name "libfido2")
    (version "1.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Yubico/libfido2")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256 (base32 "1nk4irmdg36930lgc892qmlmd4whz4fq37wknkdx5ap57i5x18i6"))))
    (native-inputs (list pkg-config))
    (inputs (list eudev libcbor openssl zlib))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      `(list (string-append
              "-DPKG_CONFIG_EXECUTABLE="
              (search-input-file %build-inputs
                                 (string-append
                                  "/bin/" ,(pkg-config-for-target)))))
      #:phases
      #~(modify-phases %standard-phases
          ;; regress tests enabled only for debug builds
          (delete 'check)
          (add-after 'unpack
              'export-udev-rule
            (lambda _
              (install-file "udev/70-u2f.rules"
                            (string-append #$output "/udev/rules.d/")))))))
    (synopsis "Library functionality and command-line tools for FIDO devices")
    (description "libfido2 provides library functionality and command-line
tools to communicate with a FIDO device over USB, and to verify attestation
and assertion signatures.

libfido2 supports the FIDO U2F (CTAP 1) and FIDO 2.0 (CTAP 2) protocols.")
    (license license:bsd-2)
    (home-page "https://github.com/Yubico/libfido2")))
