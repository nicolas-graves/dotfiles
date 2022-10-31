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

(define-public ssh-to-age
  (let* ((commit "37365ce80fa64d8794855ec3c63cc9a071799fea")
         (revision "0"))
    (package
      (name "ssh-to-age")
      (version (git-version "1.0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Mic92/ssh-to-age")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1fk2vxa854jnnffcw4q3vm1445jk1ck1v3p4mr9fh04yz06g7d28"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/Mic92/ssh-to-age"))
      (inputs (list go-golang-org-x-crypto
                    go-filippo-io-edwards25519
                    go-filippo-io-age))
      (home-page "https://github.com/Mic92/ssh-to-age")
      (synopsis "Convert SSH @code{ed25519} keys to @code{age} keys.")
      (description "This package provides a simple command-line tool to
convert SSH @code{ed25519} keys to @code{age} keys.")
      (license license:expat))))
