(define-module (packages cryptography)
  #:use-module (gnu packages)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages password-utils)

  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system go)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:))

(define-public rust-pbp-0.4
  (package
    (name "rust-pbp")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pbp" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "17nnhvnwgc1vadvm85bjsr2jqnq36lhsq7ws3wn588dnpbdf0afc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-development-inputs
       (("rust-sha2" ,rust-sha2-0.6)
        ("rust-rand-0.5.4" ,rust-rand-0.5))
       #:cargo-inputs (("rust-base64" ,rust-base64-0.9)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-digest" ,rust-digest-0.7)
                       ("rust-ed25519-dalek" ,rust-ed25519-dalek-0.7)
                       ("rust-failure" ,rust-failure-0.1)
                       ("rust-sha1" ,rust-sha1-0.2)
                       ("rust-typenum" ,rust-typenum-1))))
    (home-page "https://github.com/withoutboats/pbp")
    (synopsis "bridge non-PGP system to PGP data format")
    (description "bridge non-PGP system to PGP data format")
    (license (list license:expat license:asl2.0))))

(define-public rust-subtle-0.6
  (package
    (name "rust-subtle")
    (version "0.6.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "subtle" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0z8i79ni7zrdvplyzr5vd6b2a88jmsack886g3mh7k59x6vdvlmk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://dalek.rs/")
    (synopsis
     "Pure-Rust traits and utilities for constant-time cryptographic implementations.")
    (description
     "Pure-Rust traits and utilities for constant-time cryptographic implementations.")
    (license license:bsd-3)))

(define-public rust-clear-on-drop-0.2
  (package
    (name "rust-clear-on-drop")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "clear_on_drop" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "05qxifm7pkms9gnvr50j67x59m3br5fg68yfcrmv9zr7w40nh9wp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/cesarb/clear_on_drop")
    (synopsis "Helpers for clearing sensitive data on the stack and heap")
    (description "Helpers for clearing sensitive data on the stack and heap")
    (license (list license:expat license:asl2.0))))

(define-public rust-curve25519-dalek-0.18
  (package
    (name "rust-curve25519-dalek")
    (version "0.18.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "curve25519-dalek" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "05dvna8ppvm9x98fmhkm4wb49lmwm7n299av3ra2c9701fgkxhxg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-clear-on-drop" ,rust-clear-on-drop-0.2)
                       ("rust-clear-on-drop" ,rust-clear-on-drop-0.2)
                       ("rust-digest" ,rust-digest-0.7)
                       ("rust-digest" ,rust-digest-0.7)
                       ("rust-generic-array" ,rust-generic-array-0.9)
                       ("rust-generic-array" ,rust-generic-array-0.9)
                       ("rust-rand" ,rust-rand-0.5)
                       ("rust-rand" ,rust-rand-0.5)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-subtle" ,rust-subtle-0.6)
                       ("rust-subtle" ,rust-subtle-0.6))))
    (home-page "https://dalek.rs/curve25519-dalek")
    (synopsis
     "A pure-Rust implementation of group operations on ristretto255 and Curve25519")
    (description
     "This package provides a pure-Rust implementation of group operations on
ristretto255 and Curve25519")
    (license license:bsd-3)))

(define-public rust-ed25519-dalek-0.7
  (package
    (name "rust-ed25519-dalek")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ed25519-dalek" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1y0nx5bgr85q2x2xl9wrpg60ili88h22bxf0z4a8zw1qx8sniajp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-curve25519-dalek" ,rust-curve25519-dalek-0.18)
                       ("rust-digest" ,rust-digest-0.7)
                       ("rust-failure" ,rust-failure-0.1)
                       ("rust-generic-array" ,rust-generic-array-0.9)
                       ("rust-rand" ,rust-rand-0.5)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sha2" ,rust-sha2-0.7))))
    (home-page "https://dalek.rs")
    (synopsis
     "Fast and efficient ed25519 EdDSA key generations, signing, and verification in pure Rust.")
    (description
     "Fast and efficient ed25519 EdDSA key generations, signing, and verification in
pure Rust.")
    (license license:bsd-3)))

(define-public bpb
  (package
    (name "bpb")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "bpb" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0x3x3r0x0bq18x4ycyl6v5gqg222vawifxbw0yml93arln5zb1ij"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-ed25519-dalek" ,rust-ed25519-dalek-0.7)
                       ("rust-failure" ,rust-failure-0.1)
                       ("rust-hex" ,rust-hex-0.3)
                       ("rust-pbp" ,rust-pbp-0.4)
                       ("rust-rand" ,rust-rand-0.5)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-sha2" ,rust-sha2-0.7)
                       ("rust-toml" ,rust-toml-0.4))))
    (home-page "https://github.com/withoutboats/bpb")
    (synopsis "boats's personal barricade")
    (description "boats's personal barricade")
    (license (list license:expat license:asl2.0))))

(define-public go-github-com-go-piv-piv-go
  (package
    (name "go-github-com-go-piv-piv-go")
    (version "1.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-piv/piv-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j2szvvwgd0ysbap42rap4f60pj4smmmrxjlx0y131l3ki6v6gdm"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/go-piv/piv-go/piv"
       #:unpack-path "github.com/go-piv/piv-go"
       #:tests? #f))
    (native-inputs (list pkg-config))
    (propagated-inputs (list pcsc-lite))
    (home-page "https://github.com/go-piv/piv-go")
    (synopsis "A Go YubiKey PIV implementation")
    (description "This is not an officially supported Google product")
    (license license:asl2.0)))

(define-public go-github-com-gopasspw-pinentry
  (package
    (name "go-github-com-gopasspw-pinentry")
    (version "0.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/gopasspw/pinentry")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0sfsp8kdm2pndiah8gypnjkzj4q5cjp23yqp99p4x33s76y1p89j"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gopasspw/pinentry"))
    (home-page "https://github.com/gopasspw/pinentry")
    (synopsis "pinentry")
    (description
     "Package pinentry implements a cross platform pinentry client.  It can be used to
obtain credentials from the user through a simple UI application.")
    (license license:expat)))

(define-public yubikey-agent
  (package
    (name "yubikey-agent")
    (version "0.1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/FiloSottile/yubikey-agent")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14s61jgcmpqh70jz0krrai8xg0xqhwmillxkij50vbsagpxjssk6"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "filippo.io/yubikey-agent"))
    (propagated-inputs
     (list go-golang-org-x-term
           go-golang-org-x-sys
           go-golang-org-x-crypto
           go-github-com-gopasspw-pinentry
           go-github-com-go-piv-piv-go
           pcsc-lite))
    (native-inputs (list pkg-config))
    (home-page "https://filippo.io/yubikey-agent")
    (synopsis "yubikey-agent")
    (description "yubikey-agent is a seamless ssh-agent for YubiKeys.")
    (license license:bsd-3)))

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
    (arguments
     '(#:install-plan
       (list '("src/password-store.sh" "/bin/")
             '("src/completion/pass.bash-completion"
               "/share/bash-completion/completions/")
             '("src/completion/pass.zsh-completion"
              "/share/zsh/site-functions/")
             ;; '("src/completion/pass.fish-completion"
              ;; "/share/fish/vendor_completions.d/")
             )))
    (home-page "https://github.com/FiloSottile/passage")
    (synopsis "A fork of the password-store encrypted password manager")
    (description "A fork of the password-store encrypted password manager")
    (license license:gpl2+)))
