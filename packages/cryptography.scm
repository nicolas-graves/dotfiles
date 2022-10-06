(define-module (packages cryptography)
  #:use-module (gnu packages)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages tls)

  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system go)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:))

(define-public rust-pbp-0.4
  (package
    (name "rust-pbp")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pbp" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17nnhvnwgc1vadvm85bjsr2jqnq36lhsq7ws3wn588dnpbdf0afc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-development-inputs
       (("rust-sha2" ,rust-sha2-0.6)
        ("rust-rand-0.5.4" ,rust-rand-0.5))
       #:cargo-inputs
       (("rust-base64" ,rust-base64-0.9)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-byteorder" ,rust-byteorder-1)
        ("rust-digest" ,rust-digest-0.7)
        ("rust-ed25519-dalek" ,rust-ed25519-dalek-0.7)
        ("rust-failure" ,rust-failure-0.1)
        ("rust-sha1" ,rust-sha1-0.2)
        ("rust-typenum" ,rust-typenum-1))))
    (home-page "https://github.com/withoutboats/pbp")
    (synopsis "Bridge non-PGP system to PGP data format")
    (description "This package provides Rust function for interacting with the
PGP data format, without installing PGP.")
    (license (list license:expat license:asl2.0))))

(define-public rust-subtle-0.6
  (package
    (name "rust-subtle")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "subtle" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0z8i79ni7zrdvplyzr5vd6b2a88jmsack886g3mh7k59x6vdvlmk"))))
    (build-system cargo-build-system)
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
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clear_on_drop" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05qxifm7pkms9gnvr50j67x59m3br5fg68yfcrmv9zr7w40nh9wp"))))
    (build-system cargo-build-system)
    (arguments `(#:cargo-inputs (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/cesarb/clear_on_drop")
    (synopsis "Helpers for clearing sensitive data on the stack and heap")
    (description "Helpers for clearing sensitive data on the stack and heap")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-cbor-0.6
  (package
    (name "rust-serde-cbor")
    (version "0.6.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "serde-cbor" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1fhlz0y405f8z375dg89x7rn7mjr9za0v93p5mwk1222i3q1q617"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-bytes" ,rust-serde-bytes-0.10))
       #:cargo-development-inputs (("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://github.com/pyfisch/cbor")
    (synopsis "CBOR support for serde.")
    (description "CBOR support for serde.")
    (license (list license:expat license:asl2.0))))

(define-public rust-curve25519-dalek-0.18
  (package
    (name "rust-curve25519-dalek")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "curve25519-dalek" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05dvna8ppvm9x98fmhkm4wb49lmwm7n299av3ra2c9701fgkxhxg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         "--skip=scalar::test::batch_invert_with_a_zero_input_panics")
       #:cargo-development-inputs
       (("rust-sha2" ,rust-sha2-0.7)
        ("rust-serde-cbor" ,rust-serde-cbor-0.6)
        ("rust-criterion" ,rust-criterion-0.2))
       #:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-clear-on-drop" ,rust-clear-on-drop-0.2)
        ("rust-digest" ,rust-digest-0.7)
        ("rust-generic-array" ,rust-generic-array-0.9)
        ("rust-rand" ,rust-rand-0.5)
        ("rust-serde" ,rust-serde-1)
        ("rust-subtle" ,rust-subtle-0.6))))
    (home-page "https://dalek.rs/curve25519-dalek")
    (synopsis
     "Group operations on ristretto255 and Curve25519")
    (description
     "This package provides a pure-Rust implementation of group operations on
ristretto255 and Curve25519")
    (license license:bsd-3)))

(define-public rust-ed25519-dalek-0.7
  (package
    (name "rust-ed25519-dalek")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ed25519-dalek" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1y0nx5bgr85q2x2xl9wrpg60ili88h22bxf0z4a8zw1qx8sniajp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         "--skip=ed25519::test::golden")
       #:cargo-development-inputs
       (("rust-hex" ,rust-hex-0.3)
        ("rust-sha2" ,rust-sha2-0.7)
        ("rust-bincode" ,rust-bincode-1)
        ("rust-criterion" ,rust-criterion-0.2))
       #:cargo-inputs
       (("rust-curve25519-dalek" ,rust-curve25519-dalek-0.18)
        ("rust-digest" ,rust-digest-0.7)
        ("rust-failure" ,rust-failure-0.1)
        ("rust-generic-array" ,rust-generic-array-0.9)
        ("rust-rand" ,rust-rand-0.5)
        ("rust-serde" ,rust-serde-1)
        ("rust-sha2" ,rust-sha2-0.7))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack
             'looser-bincode-dependency
           (lambda _
             (substitute* "Cargo.toml"
               (("version = \"\\^0\\.9\"")
                "version = \"^1\" ")))))))
    (home-page "https://dalek.rs")
    (synopsis
     "ed25519 EdDSA key generations, signing, and verification")
    (description
     "Fast and efficient ed25519 EdDSA key generations, signing, and verification in
pure Rust.")
    (license license:bsd-3)))

(define-public bpb
  (package
    (name "bpb")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bpb" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0x3x3r0x0bq18x4ycyl6v5gqg222vawifxbw0yml93arln5zb1ij"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ed25519-dalek" ,rust-ed25519-dalek-0.7)
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
    (description "This package provides an opiniated tool to automatically
sign git commits, replacing gpg for that purpose.")
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

(define-public go-filippo-io-age
  (package
    (name "go-filippo-io-age")
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
    (arguments
     `(#:import-path "filippo.io/age"))
    (propagated-inputs
     (list go-golang-org-x-sys
           go-golang-org-x-term
           go-golang-org-x-crypto
           go-filippo-io-edwards25519))
    (home-page "https://filippo.io/age")
    (synopsis "Usage")
    (description
     "Package age implements file encryption according to the age-encryption.org/v1
specification.")
    (license license:bsd-3)))

(define-public go-filippo-io-cmd-age-keygen
  (package
    (inherit go-filippo-io-age)
    (name "go-filippo-io-cmd-age-keygen")
    (arguments
     `(#:import-path "filippo.io/age/cmd/age-keygen"
       #:unpack-path "filippo.io/age"))))

(define-public go-filippo-io-cmd-age
  (package
    (inherit go-filippo-io-age)
    (name "go-filippo-io-cmd-age")
    (arguments
     `(#:import-path "filippo.io/age/cmd/age"
       #:unpack-path "filippo.io/age"))))

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
     (list go-filippo-io-cmd-age-keygen go-filippo-io-cmd-age))
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
