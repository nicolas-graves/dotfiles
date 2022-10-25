(define-module (packages crates-cryptography)
  #:use-module (gnu packages)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages pkg-config)
  #:use-module (nongnu packages mozilla)
  #:use-module (packages swayr)

  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module ((guix licenses) #:prefix license:))

(define-public rust-zeroize-1.4
  (package
    (name "rust-zeroize")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zeroize" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "061d5bq9657izmcyasysh8n5k56pv5nalx4b6afqfb6b6w9ydbzf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-zeroize-derive" ,rust-zeroize-derive-1))))
    (home-page "https://github.com/iqlusioninc/crates/")
    (synopsis "Securely clear secrets from memory")
    (description
     "Zeroize securely clears secrets from memory with a simple trait built on
stable Rust primitives, which guarantee memory is zeroed using an operation
will not be ``optimized away'' by the compiler.  It uses a portable pure Rust
implementation that works everywhere, even WASM!")
    (license (list license:asl2.0 license:expat))))

(define-public rust-secrecy-0.8
  (package
    (name "rust-secrecy")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "secrecy" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "07p9h2bpkkg61f1fzzdqqbf74kwv1gg095r1cdmjzzbcl17cblcv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-zeroize" ,rust-zeroize-1.4))))
    (home-page "https://github.com/iqlusioninc/crates/")
    (synopsis
     "Wrapper types and traits for secret management which help ensure
they aren't accidentally copied, logged, or otherwise exposed
(as much as possible), and also ensure secrets are securely wiped
from memory when dropped.
")
    (description
     "Wrapper types and traits for secret management which help ensure they aren't
accidentally copied, logged, or otherwise exposed (as much as possible), and
also ensure secrets are securely wiped from memory when dropped.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-secrecy-0.6
  (package
    (name "rust-secrecy")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "secrecy" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "03q7h4yswpbrgxgn6wk9dyhilqhwcbhgwyy2m5vk9ps5ss72g0li"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bytes" ,rust-bytes-0.5)
        ("rust-serde" ,rust-serde-1)
        ("rust-zeroize" ,rust-zeroize-1.4))))
    (home-page "https://github.com/iqlusioninc/crates/")
    (synopsis "Wrapper types and traits for secret management")
    (description
     "This package provides wrapper types and traits for secret management
which help ensure they aren't
accidentally copied, logged, or otherwise exposed, and also ensure secrets
are securely wiped from memory when dropped.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-roff-0.1
  (package
    (name "rust-roff")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "roff" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0pk76fw9hqnvr8qbd5r8yq08zpgymk14wgkn5h2qhs54gfrlygp3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/rust-cli/roff-rs")
    (synopsis "ROFF (man page format) generation library")
    (description "ROFF (man page format) generation library")
    (license (list license:expat license:asl2.0))))

(define-public rust-man-0.3
  (package
    (name "rust-man")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "man" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1jd103brl70sh1hxm2w3n6z3pzazrznsl45cn53h3a47a5wzmxgb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-roff" ,rust-roff-0.1))))
    (home-page "https://github.com/rust-clique/man")
    (synopsis "Generate structured man pages")
    (description "Generate structured man pages")
    (license (list license:expat license:asl2.0))))
(define-public rust-subtle-encoding-0.5
  (package
    (name "rust-subtle-encoding")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "subtle-encoding" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0icrwnjs67xf4k02x3yq5rgcf4ksvm8jc1a1aknmw31kp3bixjvx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/iqlusioninc/crates/")
    (synopsis
     "Encoders and decoders for common data encodings (base64, bech32, hex)
which avoid data-dependent branching/table lookups and therefore
provide \"best effort\" constant time. Useful for encoding/decoding
secret values such as cryptographic keys.
")
    (description
     "Encoders and decoders for common data encodings (base64, bech32, hex) which
avoid data-dependent branching/table lookups and therefore provide \"best effort\"
constant time.  Useful for encoding/decoding secret values such as cryptographic
keys.")
    (license (list license:asl2.0 license:expat))))
(define-public rust-p384-0.8
  (package
    (name "rust-p384")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "p384" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0mxicxgrbbgjfi4dwimzxij6lcvxrnjjpmhsi8f8ik2c826chfzj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ecdsa" ,rust-ecdsa-0.12)
                       ("rust-elliptic-curve" ,rust-elliptic-curve-0.10)
                       ("rust-sha2" ,rust-sha2-0.9))))
    (home-page
     "https://github.com/RustCrypto/elliptic-curves/tree/master/p384")
    (synopsis
     "Pure Rust implementation of the NIST P-384 (a.k.a. secp384r1) elliptic curve
with support for ECDH, ECDSA signing/verification, and general purpose curve
arithmetic support.
")
    (description
     "Pure Rust implementation of the NIST P-384 (a.k.a.  secp384r1) elliptic curve
with support for ECDH, ECDSA signing/verification, and general purpose curve
arithmetic support.")
    (license (list license:asl2.0 license:expat))))
(define-public rust-yubikey-0.5
  (package
    (name "rust-yubikey")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "yubikey" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0xwiipis3r6sy69gx0dhf0bvbjjrv0j3wq9h8czdjx9wc7h7sap8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-cookie-factory" ,rust-cookie-factory-0.3)
                       ("rust-der-parser" ,rust-der-parser-6)
                       ("rust-des" ,rust-des-0.7)
                       ("rust-elliptic-curve" ,rust-elliptic-curve-0.10)
                       ("rust-hmac" ,rust-hmac-0.11)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-num-bigint-dig" ,rust-num-bigint-dig-0.7)
                       ("rust-num-integer" ,rust-num-integer-0.1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-p256" ,rust-p256-0.9)
                       ("rust-p384" ,rust-p384-0.8)
                       ("rust-pbkdf2" ,rust-pbkdf2-0.9)
                       ("rust-pcsc" ,rust-pcsc-2)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-rsa" ,rust-rsa-0.5)
                       ("rust-secrecy" ,rust-secrecy-0.8)
                       ("rust-sha-1" ,rust-sha-1-0.9)
                       ("rust-sha2" ,rust-sha2-0.9)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-subtle-encoding" ,rust-subtle-encoding-0.5)
                       ("rust-uuid" ,rust-uuid-0.8)
                       ("rust-x509" ,rust-x509-0.2)
                       ("rust-x509-parser" ,rust-x509-parser-0.12)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/iqlusioninc/yubikey.rs")
    (synopsis
     "Pure Rust cross-platform host-side driver for YubiKey devices from Yubico with
support for hardware-backed public-key decryption and digital signatures using
the Personal Identity Verification (PIV) application. Supports RSA (1024/2048)
or ECC (NIST P-256/P-384) algorithms e.g, PKCS#1v1.5, ECDSA
")
    (description
     "Pure Rust cross-platform host-side driver for YubiKey devices from Yubico with
support for hardware-backed public-key decryption and digital signatures using
the Personal Identity Verification (PIV) application.  Supports RSA (1024/2048)
or ECC (NIST P-256/P-384) algorithms e.g, PKCS#1v1.5, ECDSA")
    (license license:bsd-2)))
(define-public rust-x509-0.2
  (package
    (name "rust-x509")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "x509" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0xhcaqcp5kn05va2q0hs54qw0pznappmiwsk2ls337wrqfafqg6a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-cookie-factory" ,rust-cookie-factory-0.3))))
    (home-page "https://github.com/str4d/x509.rs")
    (synopsis "X.509 certificate serialization")
    (description "X.509 certificate serialization")
    (license (list license:expat license:asl2.0))))
(define-public rust-pcsc-sys-1
  (package
    (name "rust-pcsc-sys")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pcsc-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1si37v9n07r3csqcnnqn4i82j75b6dssyz0fzdg1n3rcpbnbzdz1"))))
    (build-system cargo-build-system)
    (native-inputs (list pkg-config))
    (inputs (list pcsc-lite))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/bluetech/pcsc-rust")
    (synopsis "Low-level bindings to the PC/SC C API")
    (description "Low-level bindings to the PC/SC C API")
    (license license:expat)))
(define-public rust-pcsc-2
  (package
    (name "rust-pcsc")
    (version "2.7.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pcsc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1i7xqknwfwi990nw452sr0fy3zgrl1avvdbgs3nawcx4g3gf8aby"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-pcsc-sys" ,rust-pcsc-sys-1))))
    (home-page "https://github.com/bluetech/pcsc-rust")
    (synopsis "Bindings to the PC/SC API for smart card communication")
    (description "Bindings to the PC/SC API for smart card communication")
    (license license:expat)))
(define-public rust-signature-derive-1
  (package
    (name "rust-signature-derive")
    (version "1.0.0-pre.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "signature_derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "140dc985ah274xz4y362lry51z6m0205lcngsa07dy3kbc4pdfpz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1)
                       ("rust-synstructure" ,rust-synstructure-0.12))))
    (home-page
     "https://github.com/RustCrypto/traits/tree/master/signature/derive")
    (synopsis "Custom derive support for the 'signature' crate")
    (description "Custom derive support for the 'signature' crate")
    (license (list license:asl2.0 license:expat))))
(define-public rust-signature-1
  (package
    (name "rust-signature")
    (version "1.3.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "signature" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "193i25db7gxw6jzgz917l57rsr50qy8i64gil4d0i3m5ry97i07j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-digest" ,rust-digest-0.9)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-signature-derive" ,rust-signature-derive-1))))
    (home-page "https://github.com/RustCrypto/traits/tree/master/signature")
    (synopsis
     "Traits for cryptographic signature algorithms (e.g. ECDSA, Ed25519)")
    (description
     "Traits for cryptographic signature algorithms (e.g.  ECDSA, Ed25519)")
    (license (list license:asl2.0 license:expat))))
(define-public rust-zeroize-1
  (package
    (name "rust-zeroize")
    (version "1.4.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zeroize" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "068nvl3n5hk6lfn5y24grf2c7anzzqfzjjccscq3md7rqp79v3fn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-zeroize-derive" ,rust-zeroize-derive-1))))
    (home-page "https://github.com/RustCrypto/utils/tree/master/zeroize")
    (synopsis "Securely clear secrets from memory with a simple trait built on
stable Rust primitives which guarantee memory is zeroed using an
operation will not be 'optimized away' by the compiler.
Uses a portable pure Rust implementation that works everywhere,
even WASM!
")
    (description
     "Securely clear secrets from memory with a simple trait built on stable Rust
primitives which guarantee memory is zeroed using an operation will not be
'optimized away' by the compiler.  Uses a portable pure Rust implementation that
works everywhere, even WASM!")
    (license (list license:asl2.0 license:expat))))
(define-public rust-group-0.10
  (package
    (name "rust-group")
    (version "0.10.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "group" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "04krmzgaq30pkld5fc1izswp6868nc26l4j70zc57wdq059kldhw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-ff" ,rust-ff-0.10)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-rand-xorshift" ,rust-rand-xorshift-0.3)
                       ("rust-subtle" ,rust-subtle-2))))
    (home-page "https://github.com/zkcrypto/group")
    (synopsis "Elliptic curve group traits and utilities")
    (description "Elliptic curve group traits and utilities")
    (license (list license:expat license:asl2.0))))
(define-public rust-addchain-0.2
  (package
    (name "rust-addchain")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "addchain" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0w45hpybsx9gzhlxf6x9451kycg8xwj3x8qzjnk8wqm55926jbiv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-num-bigint" ,rust-num-bigint-0.3)
                       ("rust-num-integer" ,rust-num-integer-0.1)
                       ("rust-num-traits" ,rust-num-traits-0.2))))
    (home-page "https://github.com/str4d/addchain")
    (synopsis "Generate addition chains")
    (description "Generate addition chains")
    (license (list license:expat license:asl2.0))))
(define-public rust-ff-derive-0.10
  (package
    (name "rust-ff-derive")
    (version "0.10.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ff_derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "168ka3gwm6zz75jqkzxfdjrvxfp4iq1wbfq7sjfjdgakcv1h2yg0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-addchain" ,rust-addchain-0.2)
                       ("rust-num-bigint" ,rust-num-bigint-0.3)
                       ("rust-num-integer" ,rust-num-integer-0.1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/zkcrypto/ff")
    (synopsis
     "Procedural macro library used to build custom prime field implementations")
    (description
     "Procedural macro library used to build custom prime field implementations")
    (license (list license:expat license:asl2.0))))
(define-public rust-ff-0.10
  (package
    (name "rust-ff")
    (version "0.10.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ff" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0kraavkyrjxgkqpfxv520bi1v7cybyp5jrazg8hj5hwbrlnhpx6h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitvec" ,rust-bitvec-0.22)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-ff-derive" ,rust-ff-derive-0.10)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-subtle" ,rust-subtle-2))))
    (home-page "https://github.com/zkcrypto/ff")
    (synopsis "Library for building and interfacing with finite fields")
    (description "Library for building and interfacing with finite fields")
    (license (list license:expat license:asl2.0))))
(define-public rust-elliptic-curve-0.10
  (package
    (name "rust-elliptic-curve")
    (version "0.10.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "elliptic-curve" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "06vj8cizhjf5rrv2c012py9lvk778pzvm03n7q9l1dcfrdyigjmy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64ct" ,rust-base64ct-1)
                       ("rust-crypto-bigint" ,rust-crypto-bigint-0.2)
                       ("rust-ff" ,rust-ff-0.10)
                       ("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-group" ,rust-group-0.10)
                       ("rust-hex-literal" ,rust-hex-literal-0.3)
                       ("rust-pkcs8" ,rust-pkcs8-0.7)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page
     "https://github.com/RustCrypto/traits/tree/master/elliptic-curve")
    (synopsis
     "General purpose Elliptic Curve Cryptography (ECC) support, including types
and traits for representing various elliptic curve forms, scalars, points,
and public/secret keys composed thereof.
")
    (description
     "General purpose Elliptic Curve Cryptography (ECC) support, including types and
traits for representing various elliptic curve forms, scalars, points, and
public/secret keys composed thereof.")
    (license (list license:asl2.0 license:expat))))
(define-public rust-ecdsa-0.12
  (package
    (name "rust-ecdsa")
    (version "0.12.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ecdsa" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0wjk4nmjhbr49xin68syqynhdi2hbyrbxhxmjahcfs2gbfm27vj3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-der" ,rust-der-0.4)
                       ("rust-elliptic-curve" ,rust-elliptic-curve-0.10)
                       ("rust-hmac" ,rust-hmac-0.11)
                       ("rust-signature" ,rust-signature-1))))
    (home-page "https://github.com/RustCrypto/signatures/tree/master/ecdsa")
    (synopsis
     "Pure Rust implementation of the Elliptic Curve Digital Signature Algorithm
(ECDSA) as specified in FIPS 186-4 (Digital Signature Standard), providing
RFC6979 deterministic signatures as well as support for added entropy
")
    (description
     "Pure Rust implementation of the Elliptic Curve Digital Signature Algorithm
(ECDSA) as specified in FIPS 186-4 (Digital Signature Standard), providing
RFC6979 deterministic signatures as well as support for added entropy")
    (license (list license:asl2.0 license:expat))))
(define-public rust-p256-0.9
  (package
    (name "rust-p256")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "p256" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "11ni90qif6apl497c2rbqzhxv0vipp8pnfak55kqlk5f3f73clyh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ecdsa" ,rust-ecdsa-0.12)
                       ("rust-elliptic-curve" ,rust-elliptic-curve-0.10)
                       ("rust-hex-literal" ,rust-hex-literal-0.3)
                       ("rust-sha2" ,rust-sha2-0.9))))
    (home-page
     "https://github.com/RustCrypto/elliptic-curves/tree/master/p256")
    (synopsis
     "Pure Rust implementation of the NIST P-256 (a.k.a. secp256r1, prime256v1)
elliptic curve with support for ECDH, ECDSA signing/verification, and general
purpose curve arithmetic
")
    (description
     "Pure Rust implementation of the NIST P-256 (a.k.a.  secp256r1, prime256v1)
elliptic curve with support for ECDH, ECDSA signing/verification, and general
purpose curve arithmetic")
    (license (list license:asl2.0 license:expat))))
(define-public rust-rayon-core-1
  (package
    (name "rust-rayon-core")
    (version "1.9.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rayon-core" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gv8k6612gc24kqqm4440f5qfx6gnyv2v6dj3d4libbdmjswv2r5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-crossbeam-deque" ,rust-crossbeam-deque-0.8)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                       ("rust-num-cpus" ,rust-num-cpus-1))))
    (home-page "https://github.com/rayon-rs/rayon")
    (synopsis "Core APIs for Rayon")
    (description "Core APIs for Rayon")
    (license (list license:expat license:asl2.0))))
(define-public rust-rayon-1
  (package
    (name "rust-rayon")
    (version "1.5.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rayon" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0z9sjcy1hnnvgkwx3cn1x44pf24jpwarp3172m9am2xd5rvyb6dx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-autocfg" ,rust-autocfg-1)
                       ("rust-crossbeam-deque" ,rust-crossbeam-deque-0.8)
                       ("rust-either" ,rust-either-1)
                       ("rust-rayon-core" ,rust-rayon-core-1))))
    (home-page "https://github.com/rayon-rs/rayon")
    (synopsis "Simple work-stealing parallelism for Rust")
    (description "Simple work-stealing parallelism for Rust")
    (license (list license:expat license:asl2.0))))
(define-public rust-dashmap-5
  (package
    (name "rust-dashmap")
    (version "5.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "dashmap" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0v6rfb82bxj6zn1p3x3273pi5vfqlji8vq8r84a4krpwlcslm0y0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/xacrimon/dashmap")
    (synopsis "Blazing fast concurrent HashMap for Rust.")
    (description "Blazing fast concurrent HashMap for Rust.")
    (license license:expat)))

(define-public rust-i18n-embed-fl-0.6
  (package
    (name "rust-i18n-embed-fl")
    (version "0.6.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "i18n-embed-fl" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "13p9miglh1ds0wbnhg5sz6phvsh8aki9i8s0g1rapl7rirqsj84l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dashmap" ,rust-dashmap-5)
                       ("rust-find-crate" ,rust-find-crate-0.6)
                       ("rust-fluent" ,rust-fluent-0.16)
                       ("rust-fluent-syntax" ,rust-fluent-syntax-0.11)
                       ("rust-i18n-config" ,rust-i18n-config-0.4)
                       ("rust-i18n-embed" ,rust-i18n-embed-0.13)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-proc-macro-error" ,rust-proc-macro-error-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-strsim" ,rust-strsim-0.10)
                       ("rust-syn" ,rust-syn-1)
                       ("rust-unic-langid" ,rust-unic-langid-0.9))))
    (home-page "")
    (synopsis
     "Macro to perform compile time checks when using the i18n-embed crate and the fluent localization system")
    (description
     "Macro to perform compile time checks when using the i18n-embed crate and the
fluent localization system")
    (license license:expat)))
(define-public rust-tr-0.1
  (package
    (name "rust-tr")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tr" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "144v71bc4krd972ldflpm2jcbfvqhzlwhqcldlcp05cf7xvsiqz9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gettext" ,rust-gettext-0.4)
                       ("rust-gettext-rs" ,rust-gettext-rs-0.7)
                       ("rust-lazy-static" ,rust-lazy-static-1))))
    (home-page "https://github.com/woboq/tr")
    (synopsis "tr! macro for localisation")
    (description "tr! macro for localisation")
    (license license:expat)))
(define-public rust-warp-0.3
  (package
    (name "rust-warp")
    (version "0.3.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "warp" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0vfcm8ssfqg5an3imcbcd0id4j2zpi76civ1n0c3vz264vlqnyzd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-compression" ,rust-async-compression-0.3)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-headers" ,rust-headers-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-mime-guess" ,rust-mime-guess-2)
                       ("rust-multipart" ,rust-multipart-0.18)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project" ,rust-pin-project-1)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-0.2)
                       ("rust-scoped-tls" ,rust-scoped-tls-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.23)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tokio-tungstenite" ,rust-tokio-tungstenite-0.17)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/seanmonstar/warp")
    (synopsis "serve the web at warp speeds")
    (description "serve the web at warp speeds")
    (license license:expat)))
(define-public rust-tungstenite-0.16
  (package
    (name "rust-tungstenite")
    (version "0.16.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tungstenite" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1l9s7gi9kgl4zynhbyb7737lmwaxaim4b818lwi7y95f2hx73lva"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.13)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rustls" ,rust-rustls-0.20)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
                       ("rust-sha-1" ,rust-sha-1-0.9)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-url" ,rust-url-2)
                       ("rust-utf-8" ,rust-utf-8-0.7)
                       ("rust-webpki" ,rust-webpki-0.22)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.22))))
    (home-page "https://github.com/snapview/tungstenite-rs")
    (synopsis "Lightweight stream-based WebSocket implementation")
    (description "Lightweight stream-based WebSocket implementation")
    (license (list license:expat license:asl2.0))))
(define-public rust-tokio-tungstenite-0.16
  (package
    (name "rust-tokio-tungstenite")
    (version "0.16.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio-tungstenite" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0wnadcv9q2yi7bjkdp6z0g4rk7kbdblsv613fpgjrhgwdbgkj2z8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-rustls" ,rust-rustls-0.20)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.23)
                       ("rust-tungstenite" ,rust-tungstenite-0.16)
                       ("rust-webpki" ,rust-webpki-0.22)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.22))))
    (home-page "https://github.com/snapview/tokio-tungstenite")
    (synopsis
     "Tokio binding for Tungstenite, the Lightweight stream-based WebSocket implementation")
    (description
     "Tokio binding for Tungstenite, the Lightweight stream-based WebSocket
implementation")
    (license license:expat)))
(define-public rust-simple-asn1-0.4
  (package
    (name "rust-simple-asn1")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "simple_asn1" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0jxy9as8nj65c2n27j843g4fpb95x4fjz31w6qx63q3wwlys2b39"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-num-bigint" ,rust-num-bigint-0.2)
                       ("rust-num-traits" ,rust-num-traits-0.2))))
    (home-page "https://github.com/acw/simple_asn1")
    (synopsis "A simple DER/ASN.1 encoding/decoding library.")
    (description
     "This package provides a simple DER/ASN.1 encoding/decoding library.")
    (license license:isc)))
(define-public rust-pem-0.8
  (package
    (name "rust-pem")
    (version "0.8.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pem" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1sqkzp87j6s79sjxk4n913gcmalzb2fdc75l832d0j7a3z9cnmpx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.13)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-regex" ,rust-regex-1))))
    (home-page "https://github.com/jcreekmore/pem-rs.git")
    (synopsis "Parse and encode PEM-encoded data.")
    (description "Parse and encode PEM-encoded data.")
    (license license:expat)))
(define-public rust-jsonwebtoken-7
  (package
    (name "rust-jsonwebtoken")
    (version "7.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "jsonwebtoken" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ciz205wcjcn7n6i871zz5xlbzk863b0ybgiqi7li9ipwhawraxg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.12)
                       ("rust-pem" ,rust-pem-0.8)
                       ("rust-ring" ,rust-ring-0.16)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-simple-asn1" ,rust-simple-asn1-0.4))))
    (home-page "https://github.com/Keats/jsonwebtoken")
    (synopsis "Create and decode JWTs in a strongly typed way.")
    (description "Create and decode JWTs in a strongly typed way.")
    (license license:expat)))
(define-public rust-async-lock-2
  (package
    (name "rust-async-lock")
    (version "2.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "async-lock" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1dkps300paviz3npwaq38n0sc92fv55b20mr3fizp0hp34fifyp9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-event-listener" ,rust-event-listener-2))))
    (home-page "https://github.com/smol-rs/async-lock")
    (synopsis "Async synchronization primitives")
    (description "Async synchronization primitives")
    (license (list license:asl2.0 license:expat))))
(define-public rust-async-session-3
  (package
    (name "rust-async-session")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "async-session" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0c76vazdlcs2rsxq8gd8a6wnb913vxhnfx1hyfmfpqml4gjlrnh7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-async-lock" ,rust-async-lock-2)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-bincode" ,rust-bincode-1)
                       ("rust-blake3" ,rust-blake3-0.3)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-hmac" ,rust-hmac-0.11)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sha2" ,rust-sha2-0.9))))
    (home-page "https://github.com/http-rs/async-session")
    (synopsis "Async session support with pluggable middleware")
    (description "Async session support with pluggable middleware")
    (license (list license:expat license:asl2.0))))
(define-public rust-salvo-extra-0.16
  (package
    (name "rust-salvo-extra")
    (version "0.16.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "salvo_extra" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "023wagm5mpkp1jnpggllbddqigsy5h4qnw2lk8m3j25fj61fl3iy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-compression" ,rust-async-compression-0.3)
                       ("rust-async-session" ,rust-async-session-3)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-cookie" ,rust-cookie-0.16)
                       ("rust-csrf" ,rust-csrf-0.4)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-hkdf" ,rust-hkdf-0.12)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-hyper-rustls" ,rust-hyper-rustls-0.23)
                       ("rust-jsonwebtoken" ,rust-jsonwebtoken-7)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project" ,rust-pin-project-1)
                       ("rust-salvo-core" ,rust-salvo-core-0.16)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tokio-tungstenite" ,rust-tokio-tungstenite-0.16)
                       ("rust-tokio-util" ,rust-tokio-util-0.6)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://salvo.rs")
    (synopsis
     "Salvo is a powerful and simplest web server framework in Rust world.
")
    (description
     "Salvo is a powerful and simplest web server framework in Rust world.")
    (license (list license:expat license:asl2.0))))
(define-public rust-textnonce-1
  (package
    (name "rust-textnonce")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "textnonce" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "10v653sz0305dlzdqh6wh795hxypk24s21iiqcfyv16p1kbzhhvp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.12)
                       ("rust-rand" ,rust-rand-0.7)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/mikedilger/textnonce")
    (synopsis "Text based random nonce generator")
    (description "Text based random nonce generator")
    (license (list license:expat license:asl2.0))))
(define-public rust-proc-quote-impl-0.3
  (package
    (name "rust-proc-quote-impl")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "proc-quote-impl" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "184ax14pyazv5g6yma60ls7x4hd5q6wah1kf677xng06idifrcvz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro-hack" ,rust-proc-macro-hack-0.5)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1))))
    (home-page "https://github.com/Goncalerta/proc-quote")
    (synopsis "A procedural macro implementation of quote!.")
    (description
     "This package provides a procedural macro implementation of quote!.")
    (license (list license:expat license:asl2.0))))
(define-public rust-proc-quote-0.4
  (package
    (name "rust-proc-quote")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "proc-quote" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0051nax31x1yzr1imbp200l2gpz6pqcmlcna099r33773lbap12y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro-hack" ,rust-proc-macro-hack-0.5)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-proc-quote-impl" ,rust-proc-quote-impl-0.3)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/Goncalerta/proc-quote")
    (synopsis "A procedural macro implementation of quote!.")
    (description
     "This package provides a procedural macro implementation of quote!.")
    (license (list license:expat license:asl2.0))))
(define-public rust-salvo-macros-0.16
  (package
    (name "rust-salvo-macros")
    (version "0.16.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "salvo_macros" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0hdlzvcv2vvbr60w1kmfr9bx8glx4xs9g0ry1pwa7yf7ig987z90"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro-crate" ,rust-proc-macro-crate-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-proc-quote" ,rust-proc-quote-0.4)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://salvo.rs")
    (synopsis "salvo proc macros")
    (description "salvo proc macros")
    (license (list license:expat license:asl2.0))))
(define-public rust-salvo-core-0.16
  (package
    (name "rust-salvo-core")
    (version "0.16.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "salvo_core" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "01dazprfzmjmvwgcrvqxjd12hgwwlk71mskwyl4cj2y2gm5p80bv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-async-compression" ,rust-async-compression-0.3)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-cookie" ,rust-cookie-0.16)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-fastrand" ,rust-fastrand-1)
                       ("rust-form-urlencoded" ,rust-form-urlencoded-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-headers" ,rust-headers-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-mime-guess" ,rust-mime-guess-2)
                       ("rust-multer" ,rust-multer-2)
                       ("rust-multimap" ,rust-multimap-0.8)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-pin-utils" ,rust-pin-utils-0.1)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-0.2)
                       ("rust-salvo-macros" ,rust-salvo-macros-0.16)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-tempdir" ,rust-tempdir-0.3)
                       ("rust-textnonce" ,rust-textnonce-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.23)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://salvo.rs")
    (synopsis
     "Salvo is a powerful and simplest web server framework in Rust world.
")
    (description
     "Salvo is a powerful and simplest web server framework in Rust world.")
    (license (list license:expat license:asl2.0))))
(define-public rust-salvo-0.16
  (package
    (name "rust-salvo")
    (version "0.16.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "salvo" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1jw9h9aac4ms9shvssc8mw53q9842f5bfqv1a8aqkpcyd2j23n4b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-salvo-core" ,rust-salvo-core-0.16)
                       ("rust-salvo-extra" ,rust-salvo-extra-0.16))))
    (home-page "https://salvo.rs")
    (synopsis
     "Salvo is a powerful and simplest web server framework in Rust world.
")
    (description
     "Salvo is a powerful and simplest web server framework in Rust world.")
    (license (list license:expat license:asl2.0))))
(define-public rust-sha2-0.10
  (package
    (name "rust-sha2")
    (version "0.10.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sha2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1h5xrrv2y06kr1gsz4pwrm3lsp206nm2gjxgbf21wfrfzsavgrl2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cpufeatures" ,rust-cpufeatures-0.2)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-sha2-asm" ,rust-sha2-asm-0.6))))
    (home-page "https://github.com/RustCrypto/hashes")
    (synopsis "Pure Rust implementation of the SHA-2 hash function family
including SHA-224, SHA-256, SHA-384, and SHA-512.
")
    (description
     "Pure Rust implementation of the SHA-2 hash function family including SHA-224,
SHA-256, SHA-384, and SHA-512.")
    (license (list license:expat license:asl2.0))))
(define-public rust-globset-0.4
  (package
    (name "rust-globset")
    (version "0.4.9")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "globset" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "02mx4yhpxfbv74pqfch0asicc868nszazhk4m4hvrv8r4qs1f7ha"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aho-corasick" ,rust-aho-corasick-0.7)
                       ("rust-bstr" ,rust-bstr-0.2)
                       ("rust-fnv" ,rust-fnv-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page
     "https://github.com/BurntSushi/ripgrep/tree/master/crates/globset")
    (synopsis
     "Cross platform single glob and glob set matching. Glob set matching is the
process of matching one or more glob patterns against a single candidate path
simultaneously, and returning all of the globs that matched.
")
    (description
     "Cross platform single glob and glob set matching.  Glob set matching is the
process of matching one or more glob patterns against a single candidate path
simultaneously, and returning all of the globs that matched.")
    (license (list license:unlicense license:expat))))
(define-public rust-rust-embed-utils-7
  (package
    (name "rust-rust-embed-utils")
    (version "7.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rust-embed-utils" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0m7h33gabh4afgf2w5i2paq94qbc2jz8nsw5wbwbbldbvy0rsrn1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-globset" ,rust-globset-0.4)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/pyros2097/rust-embed")
    (synopsis "Utilities for rust-embed")
    (description "Utilities for rust-embed")
    (license license:expat)))
(define-public rust-rust-embed-impl-6
  (package
    (name "rust-rust-embed-impl")
    (version "6.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rust-embed-impl" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "04kf6i5jzdymsf4kbi0am6wglcs382ilm0p013648d975r07npg3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-rust-embed-utils" ,rust-rust-embed-utils-7)
                       ("rust-shellexpand" ,rust-shellexpand-2)
                       ("rust-syn" ,rust-syn-1)
                       ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/pyros2097/rust-embed")
    (synopsis
     "Rust Custom Derive Macro which loads files into the rust binary at compile time during release and loads the file from the fs during dev")
    (description
     "Rust Custom Derive Macro which loads files into the rust binary at compile time
during release and loads the file from the fs during dev")
    (license license:expat)))
(define-public rust-ubyte-0.10
  (package
    (name "rust-ubyte")
    (version "0.10.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ubyte" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1rlg6sr14i3rd4kfhrwd7b7w7krlg6kpjxkd6vcx0si8gnp0s7y8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/SergioBenitez/ubyte")
    (synopsis
     "A simple, complete, const-everything, saturating, human-friendly, no_std library for byte units.
")
    (description
     "This package provides a simple, complete, const-everything, saturating,
human-friendly, no_std library for byte units.")
    (license (list license:expat license:asl2.0))))
(define-public rust-state-0.5
  (package
    (name "rust-state")
    (version "0.5.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "state" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0fzji31ijbkimbzdy4dln9mp5xp7lm1a0dnqxv4n10hywphnds6v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-loom" ,rust-loom-0.5))))
    (home-page "https://github.com/SergioBenitez/state")
    (synopsis
     "A library for safe and effortless global and thread-local state management.
")
    (description
     "This package provides a library for safe and effortless global and thread-local
state management.")
    (license (list license:expat license:asl2.0))))
(define-public rust-stable-pattern-0.1
  (package
    (name "rust-stable-pattern")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "stable-pattern" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0i8hq82vm82mqj02qqcsd7caibrih7x5w3a1xpm8hpv30261cr25"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-memchr" ,rust-memchr-2))))
    (home-page "https://github.com/SergioBenitez/stable-pattern")
    (synopsis "Stable port of std::str::Pattern and friends.")
    (description "Stable port of std::str::Pattern and friends.")
    (license (list license:expat license:asl2.0))))
(define-public rust-rocket-http-0.5
  (package
    (name "rust-rocket-http")
    (version "0.5.0-rc.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rocket_http" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "18hpzjmgvl4ibgk62i4qcpq949qsp3s0nqvi4k0y6kcm4z8nbv9d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cookie" ,rust-cookie-0.16)
                       ("rust-either" ,rust-either-1)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-pear" ,rust-pear-0.2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-ref-cast" ,rust-ref-cast-1)
                       ("rust-rustls" ,rust-rustls-0.20)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-stable-pattern" ,rust-stable-pattern-0.1)
                       ("rust-state" ,rust-state-0.5)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.23)
                       ("rust-uncased" ,rust-uncased-0.9)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-x509-parser" ,rust-x509-parser-0.13))))
    (home-page "https://rocket.rs")
    (synopsis
     "Types, traits, and parsers for HTTP requests, responses, and headers.
")
    (description
     "Types, traits, and parsers for HTTP requests, responses, and headers.")
    (license (list license:expat license:asl2.0))))
(define-public rust-devise-core-0.3
  (package
    (name "rust-devise-core")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "devise_core" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1l00qiih4z14ai0c3s16nlvw0kv4p07ygi6a0ms0knc78xpz87l4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-proc-macro2-diagnostics" ,rust-proc-macro2-diagnostics-0.9)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/SergioBenitez/Devise")
    (synopsis "A library for devising derives and other procedural macros.")
    (description
     "This package provides a library for devising derives and other procedural
macros.")
    (license (list license:expat license:asl2.0))))
(define-public rust-devise-codegen-0.3
  (package
    (name "rust-devise-codegen")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "devise_codegen" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1cp7nnfwvjp6wfq11n0ffjjrwfa1wbsb58g1bz3ha6z5lvkp6g0j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-devise-core" ,rust-devise-core-0.3)
                       ("rust-quote" ,rust-quote-1))))
    (home-page "https://github.com/SergioBenitez/Devise")
    (synopsis "A library for devising derives and other procedural macros.")
    (description
     "This package provides a library for devising derives and other procedural
macros.")
    (license (list license:expat license:asl2.0))))
(define-public rust-devise-0.3
  (package
    (name "rust-devise")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "devise" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "15dmibnykic2a1ndi66shyvxmpfysnhf05lg2iv8871g0w5miish"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-devise-codegen" ,rust-devise-codegen-0.3)
                       ("rust-devise-core" ,rust-devise-core-0.3))))
    (home-page "https://github.com/SergioBenitez/Devise")
    (synopsis "A library for devising derives and other procedural macros.")
    (description
     "This package provides a library for devising derives and other procedural
macros.")
    (license (list license:expat license:asl2.0))))
(define-public rust-rocket-codegen-0.5
  (package
    (name "rust-rocket-codegen")
    (version "0.5.0-rc.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rocket_codegen" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0iwvk69rsbww6j5r1r8mqr66mxrnpxks43np00ncvsb1kjxvdbnn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-devise" ,rust-devise-0.3)
                       ("rust-glob" ,rust-glob-0.3)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-rocket-http" ,rust-rocket-http-0.5)
                       ("rust-syn" ,rust-syn-1)
                       ("rust-unicode-xid" ,rust-unicode-xid-0.2))))
    (home-page "https://rocket.rs")
    (synopsis "Procedural macros for the Rocket web framework.")
    (description "Procedural macros for the Rocket web framework.")
    (license (list license:expat license:asl2.0))))
(define-public rust-rmp-0.8
  (package
    (name "rust-rmp")
    (version "0.8.11")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rmp" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "17rw803xv84csxgd654g7q64kqf9zgkvhsn8as3dbmlg6mr92la4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-paste" ,rust-paste-1))))
    (home-page "https://github.com/3Hren/msgpack-rust")
    (synopsis "Pure Rust MessagePack serialization implementation")
    (description "Pure Rust MessagePack serialization implementation")
    (license license:expat)))
(define-public rust-rmp-serde-1
  (package
    (name "rust-rmp-serde")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rmp-serde" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0glisa0pcj56dhsaqp5vkqkcqqnb2dcal8kjzf50n8p0jbhkpcf5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-rmp" ,rust-rmp-0.8)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/3Hren/msgpack-rust")
    (synopsis "Serde bindings for RMP")
    (description "Serde bindings for RMP")
    (license license:expat)))
(define-public rust-unsafe-libyaml-0.2
  (package
    (name "rust-unsafe-libyaml")
    (version "0.2.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "unsafe-libyaml" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0s3f83hy8rd4q6r0dj4pmwyrgvlhsd0vxmzqaslg3ica7mbzmrf1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/dtolnay/unsafe-libyaml")
    (synopsis "libyaml transpiled to rust by c2rust")
    (description "libyaml transpiled to rust by c2rust")
    (license license:expat)))
(define-public rust-serde-derive-1
  (package
    (name "rust-serde-derive")
    (version "1.0.145")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "serde_derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0k1baapz2qbd9i6x08saryngdc7m47z0syi79kcarg6isf21byl1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://serde.rs")
    (synopsis "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
     "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (license (list license:expat license:asl2.0))))
(define-public rust-serde-1
  (package
    (name "rust-serde")
    (version "1.0.145")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "serde" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "12snfm7vwzmj377aifjs570wr49glz2zzpv06scwpg1h2hsvd3kj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://serde.rs")
    (synopsis "A generic serialization/deserialization framework")
    (description
     "This package provides a generic serialization/deserialization framework")
    (license (list license:expat license:asl2.0))))
(define-public rust-rustc-rayon-core-0.4
  (package
    (name "rust-rustc-rayon-core")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rustc-rayon-core" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0c4cf58056ya3282c24bnyq39cwm1rd1m96lymfbb6yvl12929h2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-crossbeam-deque" ,rust-crossbeam-deque-0.8)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                       ("rust-num-cpus" ,rust-num-cpus-1))))
    (home-page "https://github.com/rust-lang/rustc-rayon")
    (synopsis "Core APIs for Rayon - fork for rustc")
    (description "Core APIs for Rayon - fork for rustc")
    (license (list license:expat license:asl2.0))))
(define-public rust-crossbeam-deque-0.8
  (package
    (name "rust-crossbeam-deque")
    (version "0.8.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "crossbeam-deque" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1z6ifz35lyk0mw818xcl3brgss2k8islhgdmfk9s5fwjnr982pki"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-crossbeam-epoch" ,rust-crossbeam-epoch-0.9)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8))))
    (home-page
     "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-deque")
    (synopsis "Concurrent work-stealing deque")
    (description "Concurrent work-stealing deque")
    (license (list license:expat license:asl2.0))))
(define-public rust-rustc-rayon-0.4
  (package
    (name "rust-rustc-rayon")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rustc-rayon" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ykjr1i56jmi8ykkcr7x555wnxki1vsi703mz6n2x7k0naqg0y8s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-autocfg" ,rust-autocfg-1)
                       ("rust-crossbeam-deque" ,rust-crossbeam-deque-0.8)
                       ("rust-either" ,rust-either-1)
                       ("rust-rustc-rayon-core" ,rust-rustc-rayon-core-0.4))))
    (home-page "https://github.com/rust-lang/rustc-rayon")
    (synopsis "Simple work-stealing parallelism for Rust - fork for rustc")
    (description "Simple work-stealing parallelism for Rust - fork for rustc")
    (license (list license:expat license:asl2.0))))
(define-public rust-indexmap-1
  (package
    (name "rust-indexmap")
    (version "1.9.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "indexmap" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "07nli1wcz7m81svvig8l5j6vjycjnv9va46lwblgy803ffbmm8qh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-autocfg" ,rust-autocfg-1)
                       ("rust-hashbrown" ,rust-hashbrown-0.12)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-rustc-rayon" ,rust-rustc-rayon-0.4)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/bluss/indexmap")
    (synopsis "A hash table with consistent order and fast iteration.")
    (description
     "This package provides a hash table with consistent order and fast iteration.")
    (license (list license:asl2.0 license:expat))))
(define-public rust-serde-yaml-0.9
  (package
    (name "rust-serde-yaml")
    (version "0.9.13")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "serde_yaml" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1lcifbsx4bkv3rj2rxnqrjwhqnpzxw4dkscdpmxyn39a869xa4w6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-indexmap" ,rust-indexmap-1)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-ryu" ,rust-ryu-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-unsafe-libyaml" ,rust-unsafe-libyaml-0.2))))
    (home-page "https://github.com/dtolnay/serde-yaml")
    (synopsis "YAML data format for Serde")
    (description "YAML data format for Serde")
    (license (list license:expat license:asl2.0))))
(define-public rust-proc-macro2-diagnostics-0.9
  (package
    (name "rust-proc-macro2-diagnostics")
    (version "0.9.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "proc-macro2-diagnostics" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1nmazlb1dkznjds7qwms7yxhi33ajc3isji2lsgx8r3lsqk9gwjb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1)
                       ("rust-version-check" ,rust-version-check-0.9)
                       ("rust-yansi" ,rust-yansi-0.5))))
    (home-page "")
    (synopsis "Diagnostics for proc-macro2.")
    (description "Diagnostics for proc-macro2.")
    (license (list license:expat license:asl2.0))))
(define-public rust-pear-codegen-0.2
  (package
    (name "rust-pear-codegen")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pear_codegen" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1l4209fi1n0wj110l12l4xpy32d1xffm61nm82vyq0r37ijcm9c2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-proc-macro2-diagnostics" ,rust-proc-macro2-diagnostics-0.9)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/SergioBenitez/Pear")
    (synopsis "A (codegen) pear is a fruit.")
    (description "This package provides a (codegen) pear is a fruit.")
    (license (list license:expat license:asl2.0))))
(define-public rust-inlinable-string-0.1
  (package
    (name "rust-inlinable-string")
    (version "0.1.15")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "inlinable_string" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ysjci8yfvxgf51z0ny2nnwhxrclhmb3vbngin8v4bznhr3ybyn8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/fitzgen/inlinable_string")
    (synopsis
     "The `inlinable_string` crate provides the `InlinableString` type -- an owned, grow-able UTF-8 string that stores small strings inline and avoids heap-allocation -- and the `StringExt` trait which abstracts string operations over both `std::string::String` and `InlinableString` (or even your own custom string type).")
    (description
     "The `inlinable_string` crate provides the `InlinableString` type -- an owned,
grow-able UTF-8 string that stores small strings inline and avoids
heap-allocation -- and the `StringExt` trait which abstracts string operations
over both `std::string::String` and `InlinableString` (or even your own custom
string type).")
    (license (list license:asl2.0 license:expat))))
(define-public rust-pear-0.2
  (package
    (name "rust-pear")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pear" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "00l7llav8cidhclx0m2gxm267pfa90c7r2x7xbinij74qm0l5r0m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-inlinable-string" ,rust-inlinable-string-0.1)
                       ("rust-pear-codegen" ,rust-pear-codegen-0.2)
                       ("rust-yansi" ,rust-yansi-0.5))))
    (home-page "https://github.com/SergioBenitez/Pear")
    (synopsis "A pear is a fruit.")
    (description "This package provides a pear is a fruit.")
    (license (list license:expat license:asl2.0))))
(define-public rust-figment-0.10
  (package
    (name "rust-figment")
    (version "0.10.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "figment" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1kv06b23pwxdqv42yzfylppi75ajasyyxdx5mnq1nbxfv5ad2fvf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-atomic" ,rust-atomic-0.5)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-pear" ,rust-pear-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-yaml" ,rust-serde-yaml-0.9)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-toml" ,rust-toml-0.5)
                       ("rust-uncased" ,rust-uncased-0.9)
                       ("rust-version-check" ,rust-version-check-0.9))))
    (home-page "https://github.com/SergioBenitez/Figment")
    (synopsis "A configuration library so con-free, it's unreal.")
    (description
     "This package provides a configuration library so con-free, it's unreal.")
    (license (list license:expat license:asl2.0))))
(define-public rust-binascii-0.1
  (package
    (name "rust-binascii")
    (version "0.1.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "binascii" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0wnaglgl72pn5ilv61q6y34w76gbg7crb8ifqk6lsxnq2gajjg9q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/naim94a/binascii-rs")
    (synopsis
     "Useful no-std binascii operations including base64, base32 and base16 (hex)")
    (description
     "Useful no-std binascii operations including base64, base32 and base16 (hex)")
    (license license:expat)))
(define-public rust-rocket-0.5
  (package
    (name "rust-rocket")
    (version "0.5.0-rc.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rocket" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "05wkp7a91ak4jgjhqkpifxh1qiv4vymhkks9ngz0b974zj1x1slq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-stream" ,rust-async-stream-0.3)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-atomic" ,rust-atomic-0.5)
                       ("rust-atty" ,rust-atty-0.2)
                       ("rust-binascii" ,rust-binascii-0.1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-either" ,rust-either-1)
                       ("rust-figment" ,rust-figment-0.10)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-multer" ,rust-multer-2)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-ref-cast" ,rust-ref-cast-1)
                       ("rust-rmp-serde" ,rust-rmp-serde-1)
                       ("rust-rocket-codegen" ,rust-rocket-codegen-0.5)
                       ("rust-rocket-http" ,rust-rocket-http-0.5)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-state" ,rust-state-0.5)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-ubyte" ,rust-ubyte-0.10)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-version-check" ,rust-version-check-0.9)
                       ("rust-yansi" ,rust-yansi-0.5))))
    (home-page "https://rocket.rs")
    (synopsis
     "Web framework for nightly with a focus on ease-of-use, expressibility, and speed.
")
    (description
     "Web framework for nightly with a focus on ease-of-use, expressibility, and
speed.")
    (license (list license:expat license:asl2.0))))
(define-public rust-oid-registry-0.4
  (package
    (name "rust-oid-registry")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "oid-registry" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0akbah3j8231ayrp2l1y5d9zmvbvqcsj0sa6s6dz6h85z8bhgqiq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-asn1-rs" ,rust-asn1-rs-0.3))))
    (home-page "https://github.com/rusticata/oid-registry")
    (synopsis "Object Identifier (OID) database")
    (description "Object Identifier (OID) database")
    (license (list license:expat license:asl2.0))))
(define-public rust-der-parser-7
  (package
    (name "rust-der-parser")
    (version "7.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "der-parser" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "10kfa2gzl3x20mwgrd43cyi79xgkqxyzcyrh0xylv4apa33qlfgy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-asn1-rs" ,rust-asn1-rs-0.3)
                       ("rust-cookie-factory" ,rust-cookie-factory-0.3)
                       ("rust-displaydoc" ,rust-displaydoc-0.2)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-rusticata-macros" ,rust-rusticata-macros-4))))
    (home-page "https://github.com/rusticata/der-parser")
    (synopsis "Parser/encoder for ASN.1 BER/DER data")
    (description "Parser/encoder for ASN.1 BER/DER data")
    (license (list license:expat license:asl2.0))))
(define-public rust-asn1-rs-derive-0.1
  (package
    (name "rust-asn1-rs-derive")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "asn1-rs-derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1gzf9vab06lk0zjvbr07axx64fndkng2s28bnj27fnwd548pb2yv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1)
                       ("rust-synstructure" ,rust-synstructure-0.12))))
    (home-page "https://github.com/rusticata/asn1-rs")
    (synopsis "Derive macros for the `asn1-rs` crate")
    (description "Derive macros for the `asn1-rs` crate")
    (license (list license:expat license:asl2.0))))
(define-public rust-asn1-rs-0.3
  (package
    (name "rust-asn1-rs")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "asn1-rs" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0czsk1nd4dx2k83f7jzkn8klx05wbmblkx1jh51i4c170akhbzrh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-asn1-rs-derive" ,rust-asn1-rs-derive-0.1)
                       ("rust-asn1-rs-impl" ,rust-asn1-rs-impl-0.1)
                       ("rust-bitvec" ,rust-bitvec-1)
                       ("rust-cookie-factory" ,rust-cookie-factory-0.3)
                       ("rust-displaydoc" ,rust-displaydoc-0.2)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-rusticata-macros" ,rust-rusticata-macros-4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3))))
    (home-page "https://github.com/rusticata/asn1-rs")
    (synopsis "Parser/encoder for ASN.1 BER/DER data")
    (description "Parser/encoder for ASN.1 BER/DER data")
    (license (list license:expat license:asl2.0))))
(define-public rust-x509-parser-0.13
  (package
    (name "rust-x509-parser")
    (version "0.13.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "x509-parser" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "077bi0xyaa8cmrqf3rrw1z6kkzscwd1nxdxgs7mgz2ambg7bmfcz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-asn1-rs" ,rust-asn1-rs-0.3)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-data-encoding" ,rust-data-encoding-2)
                       ("rust-der-parser" ,rust-der-parser-7)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-oid-registry" ,rust-oid-registry-0.4)
                       ("rust-ring" ,rust-ring-0.16)
                       ("rust-rusticata-macros" ,rust-rusticata-macros-4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3))))
    (home-page "https://github.com/rusticata/x509-parser")
    (synopsis "Parser for the X.509 v3 format (RFC 5280 certificates)")
    (description "Parser for the X.509 v3 format (RFC 5280 certificates)")
    (license (list license:expat license:asl2.0))))
(define-public rust-tokio-stream-0.1
  (package
    (name "rust-tokio-stream")
    (version "0.1.10")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio-stream" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1bsm6bjanyg1q08fvx8qimjg80q64wkhwmsk2g9l72h3pkbg5vgn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7))))
    (home-page "https://tokio.rs")
    (synopsis "Utilities to work with `Stream` and `tokio`.
")
    (description "Utilities to work with `Stream` and `tokio`.")
    (license license:expat)))
(define-public rust-tokio-openssl-0.6
  (package
    (name "rust-tokio-openssl")
    (version "0.6.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio-openssl" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "12l7a01sid095zmdkcmjnds9hwfcyjn9539r3c6b5w89g3xrz3y0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
                       ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/sfackler/tokio-openssl")
    (synopsis "An implementation of SSL streams for Tokio backed by OpenSSL
")
    (description
     "An implementation of SSL streams for Tokio backed by OpenSSL")
    (license (list license:expat license:asl2.0))))
(define-public rust-tokio-metrics-0.1
  (package
    (name "rust-tokio-metrics")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio-metrics" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1pawfs6gsgv0k950mz84fcf0vhsfk1am4bg2hhb1flwv0sh8bddw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://tokio.rs")
    (synopsis "Runtime and task level metrics for Tokio applications.
")
    (description "Runtime and task level metrics for Tokio applications.")
    (license license:expat)))
(define-public rust-thiserror-impl-1
  (package
    (name "rust-thiserror-impl")
    (version "1.0.37")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "thiserror-impl" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1fydmpksd14x1mkc24zas01qjssz8q43sbn2ywl6n527dda1fbcq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/dtolnay/thiserror")
    (synopsis "Implementation detail of the `thiserror` crate")
    (description "Implementation detail of the `thiserror` crate")
    (license (list license:expat license:asl2.0))))
(define-public rust-thiserror-1
  (package
    (name "rust-thiserror")
    (version "1.0.37")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "thiserror" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gky83x4i87gd87w3fknnp920wvk9yycp7dgkf5h3jg364vb7phh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-thiserror-impl" ,rust-thiserror-impl-1))))
    (home-page "https://github.com/dtolnay/thiserror")
    (synopsis "derive(Error)")
    (description "derive(Error)")
    (license (list license:expat license:asl2.0))))
(define-public rust-futures-codec-0.4
  (package
    (name "rust-futures-codec")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-codec" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0nzadpxhdxdlnlk2f0gfn0qbifqc3pbnzm10v4z04x8ciczxcm6f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytes" ,rust-bytes-0.5)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-pin-project" ,rust-pin-project-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-cbor" ,rust-serde-cbor-0.11)
                       ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/matthunz/futures-codec")
    (synopsis "Utilities for encoding and decoding frames using `async/await`")
    (description
     "Utilities for encoding and decoding frames using `async/await`")
    (license license:expat)))
(define-public rust-sse-codec-0.3
  (package
    (name "rust-sse-codec")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sse-codec" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0nh8b1y2k5lsvcva15da4by935bavirfpavs0d54pi2h2f0rz9c4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-0.4)
                       ("rust-bytes" ,rust-bytes-0.5)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-codec" ,rust-futures-codec-0.4)
                       ("rust-memchr" ,rust-memchr-2))))
    (home-page "https://github.com/goto-bus-stop/sse-codec")
    (synopsis "async Server-Sent Events protocol encoder/decoder")
    (description "async Server-Sent Events protocol encoder/decoder")
    (license license:mpl2.0)))
(define-public rust-rustls-pemfile-1
  (package
    (name "rust-rustls-pemfile")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rustls-pemfile" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0mdxhxp73vxh5pqk5nx2xdxg1z1xkn1yzrc6inh5mh7qagzswr08"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.13))))
    (home-page "https://github.com/rustls/pemfile")
    (synopsis "Basic .pem file parser for keys and certificates")
    (description "Basic .pem file parser for keys and certificates")
    (license (list license:asl2.0 license:isc license:expat))))
(define-public rust-rfc7239-0.1
  (package
    (name "rust-rfc7239")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rfc7239" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ixsyn8y2jfhfqnhwivgil3cvdr4jdr5s0nr7gqq3d3yryrifwq8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-uncased" ,rust-uncased-0.9))))
    (home-page "https://github.com/icewind1991/rfc7239")
    (synopsis "Parser for rfc7239 formatted Forwarded headers")
    (description "Parser for rfc7239 formatted Forwarded headers")
    (license (list license:expat license:asl2.0))))
(define-public rust-crc16-0.4
  (package
    (name "rust-crc16")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "crc16" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1zzwb5iv51wnh96532cxkk4aa8ys47rhzrjy98wqcys25ks8k01k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/blackbeam/rust-crc16")
    (synopsis "A CRC16 implementation")
    (description "This package provides a CRC16 implementation")
    (license license:expat)))
(define-public rust-async-native-tls-0.4
  (package
    (name "rust-async-native-tls")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "async-native-tls" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1zhkka5azpr03wg2bswabmwcwcqbdia17h2d17hk4wk47kn4qzfm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://docs.rs/crate/async-native-tls/")
    (synopsis "Native TLS using futures
")
    (description "Native TLS using futures")
    (license (list license:expat license:asl2.0))))
(define-public rust-redis-0.21
  (package
    (name "rust-redis")
    (version "0.21.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "redis" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "00qisc3agcn9li76pc171wz2z4s8xvli9pcywk9jm6nhd0n2a72p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arc-swap" ,rust-arc-swap-1)
                       ("rust-async-native-tls" ,rust-async-native-tls-0.4)
                       ("rust-async-std" ,rust-async-std-1)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-combine" ,rust-combine-4)
                       ("rust-crc16" ,rust-crc16-0.4)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-r2d2" ,rust-r2d2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-ryu" ,rust-ryu-1)
                       ("rust-sha1" ,rust-sha1-0.6)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/mitsuhiko/redis-rs")
    (synopsis "Redis driver for Rust.")
    (description "Redis driver for Rust.")
    (license license:bsd-3)))
(define-public rust-yasna-0.5
  (package
    (name "rust-yasna")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "yasna" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0k1gk11hq4rwlppv9f50bz8bnmgr73r66idpp7rybly96si38v9l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bit-vec" ,rust-bit-vec-0.6)
                       ("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-time" ,rust-time-0.3))))
    (home-page "https://github.com/qnighy/yasna.rs")
    (synopsis "ASN.1 library for Rust")
    (description "ASN.1 library for Rust")
    (license (list license:expat license:asl2.0))))
(define-public rust-oid-registry-0.6
  (package
    (name "rust-oid-registry")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "oid-registry" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0qb7dnrcpk02b4hasnffif1wf6rb9r2bam3fdsy4r10vzm1xljvx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-asn1-rs" ,rust-asn1-rs-0.5))))
    (home-page "https://github.com/rusticata/oid-registry")
    (synopsis "Object Identifier (OID) database")
    (description "Object Identifier (OID) database")
    (license (list license:expat license:asl2.0))))
(define-public rust-der-parser-8
  (package
    (name "rust-der-parser")
    (version "8.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "der-parser" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1h81cx4bxbdyl1f2g0p16jszqypvvrdsqd26wsddz85h1ndvrm22"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-asn1-rs" ,rust-asn1-rs-0.5)
                       ("rust-cookie-factory" ,rust-cookie-factory-0.3)
                       ("rust-displaydoc" ,rust-displaydoc-0.2)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-rusticata-macros" ,rust-rusticata-macros-4))))
    (home-page "https://github.com/rusticata/der-parser")
    (synopsis "Parser/encoder for ASN.1 BER/DER data")
    (description "Parser/encoder for ASN.1 BER/DER data")
    (license (list license:expat license:asl2.0))))
(define-public rust-displaydoc-0.2
  (package
    (name "rust-displaydoc")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "displaydoc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "11i8p5snlc1hs4g5q3wiyr75dn276l6kr0si5m7xmfa6y31mvy9v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/yaahc/displaydoc")
    (synopsis
     "A derive macro for implementing the display Trait via a doc comment and string interpolation
")
    (description
     "This package provides a derive macro for implementing the display Trait via a
doc comment and string interpolation")
    (license (list license:expat license:asl2.0))))
(define-public rust-wyz-0.5
  (package
    (name "rust-wyz")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wyz" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "03ir858jfk3sn98v3vzh33ap8s27sfgbalrv71n069wxyaa1bcrh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-once-cell" ,rust-once-cell-1)
                       ("rust-tap" ,rust-tap-1)
                       ("rust-typemap" ,rust-typemap-0.3))))
    (home-page "https://myrrlyn.net/crates/wyz")
    (synopsis "myrrlynâs utility collection")
    (description "myrrlynâs utility collection")
    (license license:expat)))
(define-public rust-radium-0.7
  (package
    (name "rust-radium")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "radium" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "02cxfi3ky3c4yhyqx9axqwhyaca804ws46nn4gc1imbk94nzycyw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/bitvecto-rs/radium")
    (synopsis "Portable interfaces for maybe-atomic types")
    (description "Portable interfaces for maybe-atomic types")
    (license license:expat)))
(define-public rust-funty-2
  (package
    (name "rust-funty")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "funty" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "177w048bm0046qlzvp33ag3ghqkqw4ncpzcm5lq36gxf2lla7mg6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/myrrlyn/funty")
    (synopsis "Trait generalization over the primitive types")
    (description "Trait generalization over the primitive types")
    (license license:expat)))
(define-public rust-bitvec-1
  (package
    (name "rust-bitvec")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "bitvec" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "173ydyj2q5vwj88k6xgjnfsshs4x9wbvjjv7sm0h36r34hn87hhv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-funty" ,rust-funty-2)
                       ("rust-radium" ,rust-radium-0.7)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tap" ,rust-tap-1)
                       ("rust-wyz" ,rust-wyz-0.5))))
    (home-page "https://bitvecto-rs.github.io/bitvec")
    (synopsis "Addresses memory by bits, for packed collections and bitfields")
    (description
     "Addresses memory by bits, for packed collections and bitfields")
    (license license:expat)))
(define-public rust-asn1-rs-impl-0.1
  (package
    (name "rust-asn1-rs-impl")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "asn1-rs-impl" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1va27bn7qxqp4wanzjlkagnynv6jnrhnwmcky2ahzb1r405p6xr7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/rusticata/asn1-rs")
    (synopsis "Implementation details for the `asn1-rs` crate")
    (description "Implementation details for the `asn1-rs` crate")
    (license (list license:expat license:asl2.0))))
(define-public rust-asn1-rs-derive-0.4
  (package
    (name "rust-asn1-rs-derive")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "asn1-rs-derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0v7fgmnzk7jjxv51grhwzcx5bf167nlqwk3vcmq7xblf5s4karbj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1)
                       ("rust-synstructure" ,rust-synstructure-0.12))))
    (home-page "https://github.com/rusticata/asn1-rs")
    (synopsis "Derive macros for the `asn1-rs` crate")
    (description "Derive macros for the `asn1-rs` crate")
    (license (list license:expat license:asl2.0))))
(define-public rust-asn1-rs-0.5
  (package
    (name "rust-asn1-rs")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "asn1-rs" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1m2xvdss113sv2zll7xihdnhvz4qlh5scwxk8c3v6ga5f31r0rng"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-asn1-rs-derive" ,rust-asn1-rs-derive-0.4)
                       ("rust-asn1-rs-impl" ,rust-asn1-rs-impl-0.1)
                       ("rust-bitvec" ,rust-bitvec-1)
                       ("rust-cookie-factory" ,rust-cookie-factory-0.3)
                       ("rust-displaydoc" ,rust-displaydoc-0.2)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-rusticata-macros" ,rust-rusticata-macros-4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3))))
    (home-page "https://github.com/rusticata/asn1-rs")
    (synopsis "Parser/encoder for ASN.1 BER/DER data")
    (description "Parser/encoder for ASN.1 BER/DER data")
    (license (list license:expat license:asl2.0))))
(define-public rust-x509-parser-0.14
  (package
    (name "rust-x509-parser")
    (version "0.14.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "x509-parser" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1j7b3xxpwik38y9rajglmhis551gj3zz5irw1vj1bqkwnsvvxv70"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-asn1-rs" ,rust-asn1-rs-0.5)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-data-encoding" ,rust-data-encoding-2)
                       ("rust-der-parser" ,rust-der-parser-8)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-oid-registry" ,rust-oid-registry-0.6)
                       ("rust-ring" ,rust-ring-0.16)
                       ("rust-rusticata-macros" ,rust-rusticata-macros-4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3))))
    (home-page "https://github.com/rusticata/x509-parser")
    (synopsis "Parser for the X.509 v3 format (RFC 5280 certificates)")
    (description "Parser for the X.509 v3 format (RFC 5280 certificates)")
    (license (list license:expat license:asl2.0))))
(define-public rust-time-macros-0.2
  (package
    (name "rust-time-macros")
    (version "0.2.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "time-macros" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "14h712p63k121cwi80x8ydn99k703wkcw2ksivd7r0addwd7nra2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/time-rs/time")
    (synopsis "Procedural macros for the time crate.")
    (description "Procedural macros for the time crate.")
    (license (list license:expat license:asl2.0))))
(define-public rust-num-threads-0.1
  (package
    (name "rust-num-threads")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "num_threads" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0i5vmffsv6g79z869flp1sja69g1gapddjagdw1k3q9f3l2cw698"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/jhpratt/num_threads")
    (synopsis
     "A minimal library that determines the number of running threads for the current process.")
    (description
     "This package provides a minimal library that determines the number of running
threads for the current process.")
    (license (list license:expat license:asl2.0))))
(define-public rust-wasm-bindgen-shared-0.2
  (package
    (name "rust-wasm-bindgen-shared")
    (version "0.2.83")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wasm-bindgen-shared" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0zzz9xfi3fp2n5ihhlq8ws7674a2ir2frvsd1d7yr4sxad2w0f0w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
     "Shared support between wasm-bindgen and wasm-bindgen cli, an internal
dependency.
")
    (description
     "Shared support between wasm-bindgen and wasm-bindgen cli, an internal
dependency.")
    (license (list license:expat license:asl2.0))))
(define-public rust-wasm-bindgen-backend-0.2
  (package
    (name "rust-wasm-bindgen-backend")
    (version "0.2.83")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wasm-bindgen-backend" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0hhigjqrb31axh7jgmb5y8akdpxqx8gvjs6ja9xmbc3r4lrzp3sc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bumpalo" ,rust-bumpalo-3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1)
                       ("rust-wasm-bindgen-shared" ,rust-wasm-bindgen-shared-0.2))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "Backend code generation of the wasm-bindgen tool
")
    (description "Backend code generation of the wasm-bindgen tool")
    (license (list license:expat license:asl2.0))))
(define-public rust-wasm-bindgen-macro-support-0.2
  (package
    (name "rust-wasm-bindgen-macro-support")
    (version "0.2.83")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wasm-bindgen-macro-support" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0g0rmawgkhfyfgjj2mvch7gvz1nzfnfmya0kgcq3xwn53l2hrg07"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1)
                       ("rust-wasm-bindgen-backend" ,rust-wasm-bindgen-backend-0.2)
                       ("rust-wasm-bindgen-shared" ,rust-wasm-bindgen-shared-0.2))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
     "The part of the implementation of the `#[wasm_bindgen]` attribute that is not in the shared backend crate
")
    (description
     "The part of the implementation of the `#[wasm_bindgen]` attribute that is not in
the shared backend crate")
    (license (list license:expat license:asl2.0))))
(define-public rust-wasm-bindgen-macro-0.2
  (package
    (name "rust-wasm-bindgen-macro")
    (version "0.2.83")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wasm-bindgen-macro" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0468wshk7bp78mnglcpmrb6m4q7x2fp9pz6ybk3wpri683wy0aq5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-quote" ,rust-quote-1)
                       ("rust-wasm-bindgen-macro-support" ,rust-wasm-bindgen-macro-support-0.2))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
     "Definition of the `#[wasm_bindgen]` attribute, an internal dependency
")
    (description
     "Definition of the `#[wasm_bindgen]` attribute, an internal dependency")
    (license (list license:expat license:asl2.0))))
(define-public rust-wasm-bindgen-0.2
  (package
    (name "rust-wasm-bindgen")
    (version "0.2.83")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wasm-bindgen" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0s3ji0k8p261glnsxi5rkd34v2pv67h96blb29yf32zcxsngbyga"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-wasm-bindgen-macro" ,rust-wasm-bindgen-macro-0.2))))
    (home-page "https://rustwasm.github.io/")
    (synopsis "Easy support for interacting between JS and Rust.
")
    (description "Easy support for interacting between JS and Rust.")
    (license (list license:expat license:asl2.0))))
(define-public rust-js-sys-0.3
  (package
    (name "rust-js-sys")
    (version "0.3.60")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "js-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0isslargvb1cd5xfk73xrxqni3p2ksharkp22swmc25zwgrrsh29"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
     "Bindings for all JS global objects and functions in all JS environments like
Node.js and browsers, built on `#[wasm_bindgen]` using the `wasm-bindgen` crate.
")
    (description
     "Bindings for all JS global objects and functions in all JS environments like
Node.js and browsers, built on `#[wasm_bindgen]` using the `wasm-bindgen` crate.")
    (license (list license:expat license:asl2.0))))
(define-public rust-time-0.3
  (package
    (name "rust-time")
    (version "0.3.15")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "time" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "035ggryin0n3x8fjqmbb8wybsbsmmvkx5bmckk1qw8ylqj2sjd6n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-itoa" ,rust-itoa-1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-num-threads" ,rust-num-threads-0.1)
                       ("rust-quickcheck" ,rust-quickcheck-1)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-time-macros" ,rust-time-macros-0.2))))
    (home-page "https://time-rs.github.io")
    (synopsis
     "Date and time library. Fully interoperable with the standard library. Mostly compatible with #![no_std].")
    (description
     "Date and time library.  Fully interoperable with the standard library.  Mostly
compatible with #![no_std].")
    (license (list license:expat license:asl2.0))))
(define-public rust-rcgen-0.9
  (package
    (name "rust-rcgen")
    (version "0.9.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rcgen" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1z4d4kq183m2d22j87g5q10kycd5qkyhlnx30xy4vghspbslg35y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-pem" ,rust-pem-1)
                       ("rust-ring" ,rust-ring-0.16)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-x509-parser" ,rust-x509-parser-0.14)
                       ("rust-yasna" ,rust-yasna-0.5)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/est31/rcgen")
    (synopsis "Rust X.509 certificate generator")
    (description "Rust X.509 certificate generator")
    (license (list license:expat license:asl2.0))))
(define-public rust-memchr-2
  (package
    (name "rust-memchr")
    (version "2.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "memchr" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0vanfk5mzs1g1syqnj03q8n0syggnhn55dq535h2wxr7rwpfbzrd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/BurntSushi/memchr")
    (synopsis "Safe interface to memchr.")
    (description "Safe interface to memchr.")
    (license (list license:unlicense license:expat))))
(define-public rust-quick-xml-0.23
  (package
    (name "rust-quick-xml")
    (version "0.23.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "quick-xml" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1slry2g2wrj38fnzj9ybzq9wjyknrfg25x5vzfpzn5b8kj2zrfhi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/tafia/quick-xml")
    (synopsis "High performance xml reader and writer")
    (description "High performance xml reader and writer")
    (license license:expat)))
(define-public rust-priority-queue-1
  (package
    (name "rust-priority-queue")
    (version "1.2.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "priority-queue" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1id9vzrypiilraw4wd5lh577prkjgyhd5vv77rdcgb7kkbcq4l41"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-autocfg" ,rust-autocfg-1)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/garro95/priority-queue")
    (synopsis
     "A Priority Queue implemented as a heap with a function to efficiently change the priority of an item.")
    (description
     "This package provides a Priority Queue implemented as a heap with a function to
efficiently change the priority of an item.")
    (license (list license:lgpl3 license:mpl2.0))))
(define-public rust-proc-macro-crate-1
  (package
    (name "rust-proc-macro-crate")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "proc-macro-crate" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1sclzva81n2lpjyfpdpdcd03f5ys9684vqap2xipbjdp1wxzr87d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-once-cell" ,rust-once-cell-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-toml" ,rust-toml-0.5))))
    (home-page "https://github.com/bkchr/proc-macro-crate")
    (synopsis "Replacement for crate (macro_rules keyword) in proc-macros
")
    (description "Replacement for crate (macro_rules keyword) in proc-macros")
    (license (list license:asl2.0 license:expat))))
(define-public rust-poem-derive-1
  (package
    (name "rust-poem-derive")
    (version "1.3.45")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "poem-derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0dlgzpnsj3w3l48axfc9v9acd85xx6x9i3x4ijwi09m2651mslwz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro-crate" ,rust-proc-macro-crate-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/poem-web/poem")
    (synopsis "Macros for poem")
    (description "Macros for poem")
    (license (list license:expat license:asl2.0))))
(define-public rust-opentelemetry-semantic-conventions-0.9
  (package
    (name "rust-opentelemetry-semantic-conventions")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "opentelemetry-semantic-conventions" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1gb21vchkkhd11nzggnhlmm9pf5ijj8jzzngn8j24h9dhdfw6p4q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-opentelemetry" ,rust-opentelemetry-0.17))))
    (home-page
     "https://github.com/open-telemetry/opentelemetry-rust/tree/main/opentelemetry-semantic-conventions")
    (synopsis "Semantic conventions for OpenTelemetry")
    (description "Semantic conventions for OpenTelemetry")
    (license license:asl2.0)))
(define-public rust-protobuf-codegen-2
  (package
    (name "rust-protobuf-codegen")
    (version "2.28.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "protobuf-codegen" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1mhpl2cs1d2sqddf097ala180il61g9axpqnzky5bxswnypn0d03"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-protobuf" ,rust-protobuf-2))))
    (home-page "https://github.com/stepancheg/rust-protobuf/")
    (synopsis
     "Code generator for rust-protobuf.

Includes a library to invoke programmatically (e. g. from `build.rs`) and `protoc-gen-rust` binary.
")
    (description
     "Code generator for rust-protobuf.

Includes a library to invoke programmatically (e.  g.  from `build.rs`) and
`protoc-gen-rust` binary.")
    (license license:expat)))
(define-public rust-protobuf-codegen-pure-2
  (package
    (name "rust-protobuf-codegen-pure")
    (version "2.28.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "protobuf-codegen-pure" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0rfqvpbbqh4pa406nda54jdl0sgagdgp274mmbpd7g4lzjcr78lm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-protobuf" ,rust-protobuf-2)
                       ("rust-protobuf-codegen" ,rust-protobuf-codegen-2))))
    (home-page
     "https://github.com/stepancheg/rust-protobuf/tree/master/protobuf-codegen-pure/")
    (synopsis "Pure-rust codegen for protobuf using protobuf-parser crate

WIP
")
    (description "Pure-rust codegen for protobuf using protobuf-parser crate

WIP")
    (license license:expat)))
(define-public rust-protobuf-2
  (package
    (name "rust-protobuf")
    (version "2.28.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "protobuf" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "154dfzjvxlpx37ha3cmp7fkhcsnyzbnfv7aisvz34x23k2gdjv8h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://github.com/stepancheg/rust-protobuf/")
    (synopsis "Rust implementation of Google protocol buffers
")
    (description "Rust implementation of Google protocol buffers")
    (license license:expat)))
(define-public rust-procfs-0.12
  (package
    (name "rust-procfs")
    (version "0.12.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "procfs" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "014i9pkhhvcl4q6hwkbnflgq5ssn2ybrlxbp6s5dkqilk5mn0h89"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/eminence/procfs")
    (synopsis "Interface to the linux procfs pseudo-filesystem")
    (description "Interface to the linux procfs pseudo-filesystem")
    (license (list license:expat license:asl2.0))))
(define-public rust-prometheus-0.13
  (package
    (name "rust-prometheus")
    (version "0.13.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "prometheus" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1378i02dq717cl5mvnw3dd4jay39kn2s98p5gxlqd49q56ybmj25"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-fnv" ,rust-fnv-1)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-procfs" ,rust-procfs-0.12)
                       ("rust-protobuf" ,rust-protobuf-2)
                       ("rust-protobuf-codegen-pure" ,rust-protobuf-codegen-pure-2)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/tikv/rust-prometheus")
    (synopsis "Prometheus instrumentation library for Rust applications.")
    (description "Prometheus instrumentation library for Rust applications.")
    (license license:asl2.0)))
(define-public rust-opentelemetry-prometheus-0.10
  (package
    (name "rust-opentelemetry-prometheus")
    (version "0.10.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "opentelemetry-prometheus" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ijab7q17lfznp2l152si4sd48a7xbdfrz6kw0nf3sww8xz9fa4k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-opentelemetry" ,rust-opentelemetry-0.17)
                       ("rust-prometheus" ,rust-prometheus-0.13)
                       ("rust-protobuf" ,rust-protobuf-2))))
    (home-page "https://github.com/open-telemetry/opentelemetry-rust")
    (synopsis "Prometheus exporter for OpenTelemetry")
    (description "Prometheus exporter for OpenTelemetry")
    (license license:asl2.0)))
(define-public rust-sluice-0.5
  (package
    (name "rust-sluice")
    (version "0.5.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sluice" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1d9ywr5039ibgaby8sc72f8fs5lpp8j5y6p3npya4jplxz000x3d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-channel" ,rust-async-channel-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-io" ,rust-futures-io-0.3))))
    (home-page "https://github.com/sagebind/sluice")
    (synopsis
     "Efficient ring buffer for byte buffers, FIFO queues, and SPSC channels")
    (description
     "Efficient ring buffer for byte buffers, FIFO queues, and SPSC channels")
    (license license:expat)))
(define-public rust-rustls-ffi-0.8
  (package
    (name "rust-rustls-ffi")
    (version "0.8.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rustls-ffi" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "06kqrvm1d5ps9pml26zdd2hm8hh20j6svwvqibpnx7m5rh3jg9cx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-num-enum" ,rust-num-enum-0.5)
                       ("rust-rustls" ,rust-rustls-0.20)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-0.2)
                       ("rust-sct" ,rust-sct-0.7)
                       ("rust-webpki" ,rust-webpki-0.22))
       #:cargo-development-inputs (("rust-cbindgen" ,rust-cbindgen-0.19))))
    (home-page "https://github.com/rustls/rustls-ffi")
    (synopsis "C-to-rustls bindings")
    (description "C-to-rustls bindings")
    (license (list license:asl2.0 license:isc license:expat))))
(define-public rust-curl-sys-0.4
  (package
    (name "rust-curl-sys")
    (version "0.4.57+curl-7.85.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "curl-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0w5fdvx7949332y61gdv3cmyl86vj2g1yan58v25df63zl4w5xf2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libnghttp2-sys" ,rust-libnghttp2-sys-0.1)
                       ("rust-libz-sys" ,rust-libz-sys-1)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-rustls-ffi" ,rust-rustls-ffi-0.8)
                       ("rust-vcpkg" ,rust-vcpkg-0.2)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/alexcrichton/curl-rust")
    (synopsis "Native bindings to the libcurl library")
    (description "Native bindings to the libcurl library")
    (license license:expat)))
(define-public rust-curl-0.4
  (package
    (name "rust-curl")
    (version "0.4.44")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "curl" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "08hsq6ssy228df56adv2wbgam05f5rw1f2wzs7mhkb678qbx36sh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-curl-sys" ,rust-curl-sys-0.4)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-openssl-probe" ,rust-openssl-probe-0.1)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
                       ("rust-schannel" ,rust-schannel-0.1)
                       ("rust-socket2" ,rust-socket2-0.4)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/alexcrichton/curl-rust")
    (synopsis "Rust bindings to libcurl for making HTTP requests")
    (description "Rust bindings to libcurl for making HTTP requests")
    (license license:expat)))
(define-public rust-castaway-0.1
  (package
    (name "rust-castaway")
    (version "0.1.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "castaway" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1xhspwy477qy5yg9c3jp713asxckjpx0vfrmz5l7r5zg7naqysd2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/sagebind/castaway")
    (synopsis
     "Safe, zero-cost downcasting for limited compile-time specialization.")
    (description
     "Safe, zero-cost downcasting for limited compile-time specialization.")
    (license license:expat)))
(define-public rust-isahc-1
  (package
    (name "rust-isahc")
    (version "1.7.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "isahc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1scfgyv3dpjbkqa9im25cd12cs6rbd8ygcaw67f3dx41sys08kik"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-channel" ,rust-async-channel-1)
                       ("rust-castaway" ,rust-castaway-0.1)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                       ("rust-curl" ,rust-curl-0.4)
                       ("rust-curl-sys" ,rust-curl-sys-0.4)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-event-listener" ,rust-event-listener-2)
                       ("rust-futures-lite" ,rust-futures-lite-1)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-httpdate" ,rust-httpdate-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.11)
                       ("rust-polling" ,rust-polling-2)
                       ("rust-publicsuffix" ,rust-publicsuffix-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-sluice" ,rust-sluice-0.5)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-tracing-futures" ,rust-tracing-futures-0.2)
                       ("rust-url" ,rust-url-2)
                       ("rust-waker-fn" ,rust-waker-fn-1))))
    (home-page "https://github.com/sagebind/isahc")
    (synopsis "The practical HTTP client that is fun to use.")
    (description "The practical HTTP client that is fun to use.")
    (license license:expat)))
(define-public rust-opentelemetry-http-0.6
  (package
    (name "rust-opentelemetry-http")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "opentelemetry-http" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0y9khrw8bc8jdjyhfx2whllkl7ywxrsrjvmygbsjh7p61qa4i424"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-isahc" ,rust-isahc-1)
                       ("rust-opentelemetry" ,rust-opentelemetry-0.17)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-surf" ,rust-surf-2))))
    (home-page "https://github.com/open-telemetry/opentelemetry-rust")
    (synopsis
     "Helper implementations for exchange of traces and metrics over HTTP")
    (description
     "Helper implementations for exchange of traces and metrics over HTTP")
    (license license:asl2.0)))
(define-public rust-opentelemetry-0.17
  (package
    (name "rust-opentelemetry")
    (version "0.17.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "opentelemetry" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1f5c04yl784bwzksl66q6vjp0fjk7dnn9ms9iksgs4xg0acfh1b1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-dashmap" ,rust-dashmap-4)
                       ("rust-fnv" ,rust-fnv-1)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-executor" ,rust-futures-executor-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project" ,rust-pin-project-1)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1))))
    (home-page "https://github.com/open-telemetry/opentelemetry-rust")
    (synopsis "A metrics collection and distributed tracing framework")
    (description
     "This package provides a metrics collection and distributed tracing framework")
    (license license:asl2.0)))
(define-public rust-hyper-rustls-0.23
  (package
    (name "rust-hyper-rustls")
    (version "0.23.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "hyper-rustls" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1b2wi75qrmwfpw3pqwcg1xjndl4z0aris15296wf7i8d5v04hz6q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-http" ,rust-http-0.2)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rustls" ,rust-rustls-0.20)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.23)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.22))))
    (home-page "https://github.com/ctz/hyper-rustls")
    (synopsis "Rustls+hyper integration for pure rust HTTPS")
    (description "Rustls+hyper integration for pure rust HTTPS")
    (license (list license:asl2.0 license:isc license:expat))))
(define-public rust-hyper-0.14
  (package
    (name "rust-hyper")
    (version "0.14.20")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "hyper" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1b7vm9dzs3hg5a6dk401n4hhg1hqh5r94lj07jh3bqrrbkf2kj82"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-h2" ,rust-h2-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-http-body" ,rust-http-body-0.4)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-httpdate" ,rust-httpdate-1)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-socket2" ,rust-socket2-0.4)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-want" ,rust-want-0.3))))
    (home-page "https://hyper.rs")
    (synopsis "A fast and correct HTTP library.")
    (description "This package provides a fast and correct HTTP library.")
    (license license:expat)))
(define-public rust-chacha20-0.7
  (package
    (name "rust-chacha20")
    (version "0.7.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "chacha20" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1c8h4sp9zh13v8p9arydjcj92xc6j3mccrjc4mizrvq7fzx9717h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cipher" ,rust-cipher-0.3)
                       ("rust-cpufeatures" ,rust-cpufeatures-0.2)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/stream-ciphers")
    (synopsis
     "The ChaCha20 stream cipher (RFC 8439) implemented in pure Rust using traits
from the RustCrypto `cipher` crate, with optional architecture-specific
hardware acceleration (AVX2, SSE2). Additionally provides the ChaCha8, ChaCha12,
XChaCha20, XChaCha12 and XChaCha8 stream ciphers, and also optional
rand_core-compatible RNGs based on those ciphers.
")
    (description
     "The ChaCha20 stream cipher (RFC 8439) implemented in pure Rust using traits from
the RustCrypto `cipher` crate, with optional architecture-specific hardware
acceleration (AVX2, SSE2).  Additionally provides the ChaCha8, ChaCha12,
XChaCha20, XChaCha12 and XChaCha8 stream ciphers, and also optional
rand_core-compatible RNGs based on those ciphers.")
    (license (list license:asl2.0 license:expat))))
(define-public rust-chacha20poly1305-0.8
  (package
    (name "rust-chacha20poly1305")
    (version "0.8.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "chacha20poly1305" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "18mb6k1w71dqv5q50an4rvp19l6yg8ssmvfrmknjfh2z0az7lm5n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aead" ,rust-aead-0.4)
                       ("rust-chacha20" ,rust-chacha20-0.7)
                       ("rust-cipher" ,rust-cipher-0.3)
                       ("rust-poly1305" ,rust-poly1305-0.7)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page
     "https://github.com/RustCrypto/AEADs/tree/master/chacha20poly1305")
    (synopsis
     "Pure Rust implementation of the ChaCha20Poly1305 Authenticated Encryption
with Additional Data Cipher (RFC 8439) with optional architecture-specific
hardware acceleration. Also contains implementations of the XChaCha20Poly1305
extended nonce variant of ChaCha20Poly1305, and the reduced-round
ChaCha8Poly1305 and ChaCha12Poly1305 lightweight variants.
")
    (description
     "Pure Rust implementation of the ChaCha20Poly1305 Authenticated Encryption with
Additional Data Cipher (RFC 8439) with optional architecture-specific hardware
acceleration.  Also contains implementations of the XChaCha20Poly1305 extended
nonce variant of ChaCha20Poly1305, and the reduced-round ChaCha8Poly1305 and
ChaCha12Poly1305 lightweight variants.")
    (license (list license:asl2.0 license:expat))))
(define-public rust-zeroize-1
  (package
    (name "rust-zeroize")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zeroize" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1z8yix823b6lz878qwg6bvwhg3lb0cbw3c9yij9p8mbv7zdzfmj7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-zeroize-derive" ,rust-zeroize-derive-1))))
    (home-page "https://github.com/RustCrypto/utils/tree/master/zeroize")
    (synopsis "Securely clear secrets from memory with a simple trait built on
stable Rust primitives which guarantee memory is zeroed using an
operation will not be 'optimized away' by the compiler.
Uses a portable pure Rust implementation that works everywhere,
even WASM!
")
    (description
     "Securely clear secrets from memory with a simple trait built on stable Rust
primitives which guarantee memory is zeroed using an operation will not be
'optimized away' by the compiler.  Uses a portable pure Rust implementation that
works everywhere, even WASM!")
    (license (list license:asl2.0 license:expat))))
(define-public rust-polyval-0.5
  (package
    (name "rust-polyval")
    (version "0.5.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "polyval" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1890wqvc0csc9y9k9k4gsbz91rgdnhn6xnfmy9pqkh674fvd46c4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cpufeatures" ,rust-cpufeatures-0.2)
                       ("rust-opaque-debug" ,rust-opaque-debug-0.3)
                       ("rust-universal-hash" ,rust-universal-hash-0.4)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/universal-hashes")
    (synopsis
     "POLYVAL is a GHASH-like universal hash over GF(2^128) useful for constructing
a Message Authentication Code (MAC)
")
    (description
     "POLYVAL is a GHASH-like universal hash over GF(2^128) useful for constructing a
Message Authentication Code (MAC)")
    (license (list license:asl2.0 license:expat))))
(define-public rust-ghash-0.4
  (package
    (name "rust-ghash")
    (version "0.4.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ghash" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "169wvrc2k9lw776x3pmqp76kc0w5717wz01bfg9rz0ypaqbcr0qm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-opaque-debug" ,rust-opaque-debug-0.3)
                       ("rust-polyval" ,rust-polyval-0.5)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/universal-hashes")
    (synopsis
     "Universal hash over GF(2^128) useful for constructing a Message Authentication Code (MAC),
as in the AES-GCM authenticated encryption cipher.
")
    (description
     "Universal hash over GF(2^128) useful for constructing a Message Authentication
Code (MAC), as in the AES-GCM authenticated encryption cipher.")
    (license (list license:asl2.0 license:expat))))
(define-public rust-aes-gcm-0.9
  (package
    (name "rust-aes-gcm")
    (version "0.9.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "aes-gcm" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1xndncn1phjb7pjam63vl0yp7h8jh95m0yxanr1092vx7al8apyz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aead" ,rust-aead-0.4)
                       ("rust-aes" ,rust-aes-0.7)
                       ("rust-cipher" ,rust-cipher-0.3)
                       ("rust-ctr" ,rust-ctr-0.8)
                       ("rust-ghash" ,rust-ghash-0.4)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/AEADs")
    (synopsis "Pure Rust implementation of the AES-GCM (Galois/Counter Mode)
Authenticated Encryption with Associated Data (AEAD) Cipher
with optional architecture-specific hardware acceleration
")
    (description
     "Pure Rust implementation of the AES-GCM (Galois/Counter Mode) Authenticated
Encryption with Associated Data (AEAD) Cipher with optional
architecture-specific hardware acceleration")
    (license (list license:asl2.0 license:expat))))
(define-public rust-aead-0.4
  (package
    (name "rust-aead")
    (version "0.4.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "aead" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0xw8kp9j1whfdxhgmr2qf9xgslkg52zh6gzmhsh13y9w3s73nq8b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-blobby" ,rust-blobby-0.3)
                       ("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-heapless" ,rust-heapless-0.7)
                       ("rust-rand-core" ,rust-rand-core-0.6))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis
     "Traits for Authenticated Encryption with Associated Data (AEAD) algorithms,
such as AES-GCM as ChaCha20Poly1305, which provide a high-level API
")
    (description
     "Traits for Authenticated Encryption with Associated Data (AEAD) algorithms, such
as AES-GCM as ChaCha20Poly1305, which provide a high-level API")
    (license (list license:expat license:asl2.0))))
(define-public rust-csrf-0.4
  (package
    (name "rust-csrf")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "csrf" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1q7ixhshj6a7x2vgsr4d4iqa5mgp4fwkr4lx2hgvnj9xcy1py9dh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aead" ,rust-aead-0.4)
                       ("rust-aes-gcm" ,rust-aes-gcm-0.9)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-chacha20poly1305" ,rust-chacha20poly1305-0.8)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-data-encoding" ,rust-data-encoding-2)
                       ("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-hmac" ,rust-hmac-0.11)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-sha2" ,rust-sha2-0.9)
                       ("rust-typemap" ,rust-typemap-0.3))))
    (home-page "https://github.com/heartsucker/rust-csrf")
    (synopsis "CSRF protection primitives")
    (description "CSRF protection primitives")
    (license license:expat)))
(define-public rust-async-compression-0.3
  (package
    (name "rust-async-compression")
    (version "0.3.14")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "async-compression" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "15gnvigh9jaq80ipn04j06k6f2vgnxjp2ddi2z3ldxq1mf9d6prl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-brotli" ,rust-brotli-3)
                       ("rust-bytes" ,rust-bytes-0.5)
                       ("rust-bzip2" ,rust-bzip2-0.4)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio" ,rust-tokio-0.2)
                       ("rust-tokio" ,rust-tokio-0.3)
                       ("rust-xz2" ,rust-xz2-0.1)
                       ("rust-zstd" ,rust-zstd-0.11)
                       ("rust-zstd-safe" ,rust-zstd-safe-5))))
    (home-page "https://github.com/Nemo157/async-compression")
    (synopsis
     "Adaptors between compression crates and Rust's modern asynchronous IO types.
")
    (description
     "Adaptors between compression crates and Rust's modern asynchronous IO types.")
    (license (list license:expat license:asl2.0))))
(define-public rust-poem-1
  (package
    (name "rust-poem")
    (version "1.3.45")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "poem" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0nn8wakgxgyzqqnjpgib7d5qkv9ck69adwy0f4320dlfj1rbm4i9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-async-compression" ,rust-async-compression-0.3)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-cookie" ,rust-cookie-0.16)
                       ("rust-csrf" ,rust-csrf-0.4)
                       ("rust-eyre" ,rust-eyre-0.6)
                       ("rust-fluent" ,rust-fluent-0.16)
                       ("rust-fluent-langneg" ,rust-fluent-langneg-0.13)
                       ("rust-fluent-syntax" ,rust-fluent-syntax-0.11)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-headers" ,rust-headers-0.3)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-httpdate" ,rust-httpdate-1)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-hyper-rustls" ,rust-hyper-rustls-0.23)
                       ("rust-intl-memoizer" ,rust-intl-memoizer-0.5)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-mime-guess" ,rust-mime-guess-2)
                       ("rust-multer" ,rust-multer-2)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-opentelemetry" ,rust-opentelemetry-0.17)
                       ("rust-opentelemetry-http" ,rust-opentelemetry-http-0.6)
                       ("rust-opentelemetry-prometheus" ,rust-opentelemetry-prometheus-0.10)
                       ("rust-opentelemetry-semantic-conventions" ,rust-opentelemetry-semantic-conventions-0.9)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-poem-derive" ,rust-poem-derive-1)
                       ("rust-priority-queue" ,rust-priority-queue-1)
                       ("rust-prometheus" ,rust-prometheus-0.13)
                       ("rust-quick-xml" ,rust-quick-xml-0.23)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rcgen" ,rust-rcgen-0.9)
                       ("rust-redis" ,rust-redis-0.21)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-rfc7239" ,rust-rfc7239-0.1)
                       ("rust-ring" ,rust-ring-0.16)
                       ("rust-rust-embed" ,rust-rust-embed-6)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-sse-codec" ,rust-sse-codec-0.3)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-metrics" ,rust-tokio-metrics-0.1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-openssl" ,rust-tokio-openssl-0.6)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.23)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tokio-tungstenite" ,rust-tokio-tungstenite-0.17)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-unic-langid" ,rust-unic-langid-0.9)
                       ("rust-x509-parser" ,rust-x509-parser-0.13))))
    (home-page "https://github.com/poem-web/poem")
    (synopsis
     "Poem is a full-featured and easy-to-use web framework with the Rust programming language.")
    (description
     "Poem is a full-featured and easy-to-use web framework with the Rust programming
language.")
    (license (list license:expat license:asl2.0))))

(define-public rust-zerocopy-derive-0.3
  (package
    (name "rust-zerocopy-derive")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zerocopy-derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "17rab2i1vwmxcr7c6r6xv55nhy41wlay0lpfcyl4vqpgh8mwiyx0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-syn" ,rust-syn-1)
                       ("rust-synstructure" ,rust-synstructure-0.12))))
    (home-page
     "https://fuchsia.googlesource.com/fuchsia/+/HEAD/src/lib/zerocopy/zerocopy-derive")
    (synopsis "Custom derive for traits from the zerocopy crate")
    (description "Custom derive for traits from the zerocopy crate")
    ;; (license unknown-license!)
    (license license:expat)))

(define-public rust-zerocopy-0.6
  (package
    (name "rust-zerocopy")
    (version "0.6.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zerocopy" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0dpj4nd9v56wy93ahjkp95znjzj91waqvidqch8gxwdwq661hbrk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-zerocopy-derive" ,rust-zerocopy-derive-0.3))))
    (home-page
     "https://fuchsia.googlesource.com/fuchsia/+/HEAD/src/lib/zerocopy")
    (synopsis "Utilities for zero-copy parsing and serialization")
    (description "Utilities for zero-copy parsing and serialization")
    ;; (license unknown-license!)
    (license license:expat)))

(define-public rust-uuid-macro-internal-1
  (package
    (name "rust-uuid-macro-internal")
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "uuid-macro-internal" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0zf96l5vby3r8qrr6ncmm1bqp0hhljy7xfvs4d8gl3lrln0p33sl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "")
    (synopsis "Private implementation details of the uuid! macro.")
    (description "Private implementation details of the uuid! macro.")
    (license (list license:asl2.0 license:expat))))
(define-public rust-sha1-smol-1
  (package
    (name "rust-sha1-smol")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sha1_smol" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "04nhbhvsk5ms1zbshs80iq5r1vjszp2xnm9f0ivj38q3dhc4f6mf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/mitsuhiko/sha1-smol")
    (synopsis "Minimal dependency free implementation of SHA1 for Rust.")
    (description "Minimal dependency free implementation of SHA1 for Rust.")
    (license license:bsd-3)))
(define-public rust-md5-asm-0.5
  (package
    (name "rust-md5-asm")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "md5-asm" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ixmkg8j7sqy9zln6pz9xi2dl2d9zpm8pz6p49za47n1bvradfbk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/RustCrypto/asm-hashes")
    (synopsis "Assembly implementation of MD5 compression function")
    (description "Assembly implementation of MD5 compression function")
    (license license:expat)))
(define-public rust-md-5-0.10
  (package
    (name "rust-md-5")
    (version "0.10.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "md-5" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1jmrykh705dfclkgxwjysj5y8l1nyrn1gddw5xpgyjyla1l50rb3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-digest" ,rust-digest-0.10)
                       ("rust-md5-asm" ,rust-md5-asm-0.5))))
    (home-page "https://github.com/RustCrypto/hashes")
    (synopsis "MD5 hash function")
    (description "MD5 hash function")
    (license (list license:expat license:asl2.0))))
(define-public rust-atomic-0.5
  (package
    (name "rust-atomic")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "atomic" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0k135q1qfmxxyzrlhr47r0j38r5fnd4163rgl552qxyagrk853dq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-autocfg" ,rust-autocfg-1))))
    (home-page "https://github.com/Amanieu/atomic-rs")
    (synopsis "Generic Atomic<T> wrapper type")
    (description "Generic Atomic<T> wrapper type")
    (license (list license:asl2.0 license:expat))))
(define-public rust-uuid-1
  (package
    (name "rust-uuid")
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "uuid" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0bwdz7bgigbh4625ga564xxbqy4srhbmzq3nqkz1ypsd67s6jr6x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-atomic" ,rust-atomic-0.5)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-md-5" ,rust-md-5-0.10)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sha1-smol" ,rust-sha1-smol-1)
                       ("rust-slog" ,rust-slog-2)
                       ("rust-uuid-macro-internal" ,rust-uuid-macro-internal-1)
                       ("rust-zerocopy" ,rust-zerocopy-0.6))))
    (home-page "https://github.com/uuid-rs/uuid")
    (synopsis "A library to generate and parse UUIDs.")
    (description
     "This package provides a library to generate and parse UUIDs.")
    (license (list license:asl2.0 license:expat))))
(define-public rust-iri-string-0.4
  (package
    (name "rust-iri-string")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "iri-string" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0y2z4f5y87hnff2d5lcl811hp7iv2f5qri7x3fgm48z2q4w7c3wg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-nom" ,rust-nom-7)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/lo48576/iri-string")
    (synopsis "IRI as string types")
    (description "IRI as string types")
    (license (list license:expat license:asl2.0))))
(define-public rust-http-range-header-0.3
  (package
    (name "rust-http-range-header")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "http-range-header" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0aas8c5dagfhcqpmqq9xw6a8nkl3lfg4g4mpddvyz1cj1bnqxzhb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/MarcusGrass/parse-range-headers")
    (synopsis "No-dep range header parser")
    (description "No-dep range header parser")
    (license license:expat)))
(define-public rust-futures-task-0.3
  (package
    (name "rust-futures-task")
    (version "0.3.24")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-task" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1wcln9clj15n7c7igm5wp9yj4kfgj526siwhyf9i51bkgi38ql56"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "Tools for working with tasks.
")
    (description "Tools for working with tasks.")
    (license (list license:expat license:asl2.0))))
(define-public rust-futures-macro-0.3
  (package
    (name "rust-futures-macro")
    (version "0.3.24")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-macro" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "05rgwsq0bdbld7hl8r4dg4x4zmv0jzbbr23yvzdh8v25qz8ibka2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The futures-rs procedural macro implementations.
")
    (description "The futures-rs procedural macro implementations.")
    (license (list license:expat license:asl2.0))))
(define-public rust-futures-io-0.3
  (package
    (name "rust-futures-io")
    (version "0.3.24")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-io" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0s7bhmfnbs8pywlhp83bixxi5fn7w73if2vwcdw4bz88lfkx5x5v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis
     "The `AsyncRead`, `AsyncWrite`, `AsyncSeek`, and `AsyncBufRead` traits for the futures-rs library.
")
    (description
     "The `AsyncRead`, `AsyncWrite`, `AsyncSeek`, and `AsyncBufRead` traits for the
futures-rs library.")
    (license (list license:expat license:asl2.0))))
(define-public rust-futures-sink-0.3
  (package
    (name "rust-futures-sink")
    (version "0.3.24")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-sink" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0mmsyw3ipg3gddm0nh95ly9sr51xc834g0vj1sipnwifm6jhpci1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The asynchronous `Sink` trait for the futures-rs library.
")
    (description "The asynchronous `Sink` trait for the futures-rs library.")
    (license (list license:expat license:asl2.0))))
(define-public rust-futures-core-0.3
  (package
    (name "rust-futures-core")
    (version "0.3.24")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-core" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1gsnrvmzddzfvywx9jnc75vxwp78jri1wlz6inw3yb1n0pga6njf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The core traits and types in for the `futures` library.
")
    (description "The core traits and types in for the `futures` library.")
    (license (list license:expat license:asl2.0))))
(define-public rust-futures-channel-0.3
  (package
    (name "rust-futures-channel")
    (version "0.3.24")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-channel" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0l5hkcwv5bsjz3vswpzlp945cjqdrggjqwfn1xfm1pgs506d5g9h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "Channels for asynchronous communication using futures-rs.
")
    (description "Channels for asynchronous communication using futures-rs.")
    (license (list license:expat license:asl2.0))))
(define-public rust-futures-util-0.3
  (package
    (name "rust-futures-util")
    (version "0.3.24")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-util" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "144wn33xk4az27xd7cgxrx0v4qygmxm52qmj8cp1vk31psqnrys4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures" ,rust-futures-0.1)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-macro" ,rust-futures-macro-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-futures-task" ,rust-futures-task-0.3)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-pin-utils" ,rust-pin-utils-0.1)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-tokio-io" ,rust-tokio-io-0.1))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis
     "Common utilities and extension traits for the futures-rs library.
")
    (description
     "Common utilities and extension traits for the futures-rs library.")
    (license (list license:expat license:asl2.0))))
(define-public rust-tower-http-0.3
  (package
    (name "rust-tower-http")
    (version "0.3.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tower-http" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1fp3mw79g38i3gfdcz9d72v5ysqiz8v1aqzfmj7zkny1fn30qlrw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-compression" ,rust-async-compression-0.3)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-http-body" ,rust-http-body-0.4)
                       ("rust-http-range-header" ,rust-http-range-header-0.3)
                       ("rust-httpdate" ,rust-httpdate-1)
                       ("rust-iri-string" ,rust-iri-string-0.4)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-mime-guess" ,rust-mime-guess-2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/tower-rs/tower-http")
    (synopsis "Tower middleware and utilities for HTTP clients and servers")
    (description "Tower middleware and utilities for HTTP clients and servers")
    (license license:expat)))
(define-public rust-tower-service-0.3
  (package
    (name "rust-tower-service")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tower-service" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0lmfzmmvid2yp2l36mbavhmqgsvzqf7r2wiwz73ml4xmwaf1rg5n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/tower-rs/tower")
    (synopsis
     "Trait representing an asynchronous, request / response based, client or server.
")
    (description
     "Trait representing an asynchronous, request / response based, client or server.")
    (license license:expat)))
(define-public rust-hdrhistogram-7
  (package
    (name "rust-hdrhistogram")
    (version "7.5.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "hdrhistogram" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1a1al1rfxcqmx0n9h100ggvg036f4rv69fq12kimazvw9zsvj6bz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.13)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-num-traits" ,rust-num-traits-0.2))))
    (home-page "https://github.com/HdrHistogram/HdrHistogram_rust")
    (synopsis "A port of HdrHistogram to Rust")
    (description "This package provides a port of HdrHistogram to Rust")
    (license (list license:expat license:asl2.0))))
(define-public rust-tower-0.4
  (package
    (name "rust-tower")
    (version "0.4.13")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tower" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "073wncyqav4sak1p755hf6vl66njgfc1z1g1di9rxx3cvvh9pymq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-hdrhistogram" ,rust-hdrhistogram-7)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-pin-project" ,rust-pin-project-1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/tower-rs/tower")
    (synopsis
     "Tower is a library of modular and reusable components for building robust
clients and servers.
")
    (description
     "Tower is a library of modular and reusable components for building robust
clients and servers.")
    (license license:expat)))
(define-public rust-tungstenite-0.17
  (package
    (name "rust-tungstenite")
    (version "0.17.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tungstenite" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1q2czb80xb7hp7ipqi5d21716i52k8s7iz18xxzfwaccdbyr4yg2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.13)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rustls" ,rust-rustls-0.20)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
                       ("rust-sha-1" ,rust-sha-1-0.10)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-url" ,rust-url-2)
                       ("rust-utf-8" ,rust-utf-8-0.7)
                       ("rust-webpki" ,rust-webpki-0.22)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.22))))
    (home-page "https://github.com/snapview/tungstenite-rs")
    (synopsis "Lightweight stream-based WebSocket implementation")
    (description "Lightweight stream-based WebSocket implementation")
    (license (list license:expat license:asl2.0))))
(define-public rust-tokio-tungstenite-0.17
  (package
    (name "rust-tokio-tungstenite")
    (version "0.17.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio-tungstenite" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "10dingfgq7ch65dzv2j0q8k3ghdf3ihl6hp0fwfl145dpqaxs57p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-rustls" ,rust-rustls-0.20)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.23)
                       ("rust-tungstenite" ,rust-tungstenite-0.17)
                       ("rust-webpki" ,rust-webpki-0.22)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.22))))
    (home-page "https://github.com/snapview/tokio-tungstenite")
    (synopsis
     "Tokio binding for Tungstenite, the Lightweight stream-based WebSocket implementation")
    (description
     "Tokio binding for Tungstenite, the Lightweight stream-based WebSocket
implementation")
    (license license:expat)))
(define-public rust-sync-wrapper-0.1
  (package
    (name "rust-sync-wrapper")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sync_wrapper" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1a59lwsw52d1a64l2y1m7npfw6xjvrjf96c5014g1b69lkj8yl90"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://docs.rs/sync_wrapper")
    (synopsis
     "A tool for enlisting the compilerâs help in proving the absence of concurrency")
    (description
     "This package provides a tool for enlisting the compilerâs help in proving the
absence of concurrency")
    (license license:asl2.0)))
(define-public rust-multer-2
  (package
    (name "rust-multer")
    (version "2.0.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "multer" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "08nzrccp8n65qsddfshxbyn82zdmzg32i3gpgajx5jx4wy61km3f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-spin" ,rust-spin-0.9)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-version-check" ,rust-version-check-0.9))))
    (home-page "https://github.com/rousan/multer-rs")
    (synopsis
     "An async parser for `multipart/form-data` content-type in Rust.")
    (description
     "An async parser for `multipart/form-data` content-type in Rust.")
    (license license:expat)))
(define-public rust-matchit-0.5
  (package
    (name "rust-matchit")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "matchit" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1swbyfxyz6nh8df514dqgds6al8lrrcxynhpbbgn5dvijrwvmjvk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/ibraheemdev/matchit")
    (synopsis "A blazing fast URL router.")
    (description "This package provides a blazing fast URL router.")
    (license license:expat)))
(define-public rust-headers-0.3
  (package
    (name "rust-headers")
    (version "0.3.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "headers" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "11258p6q2md68sfhmqrgrx23vjiapqcbxffh1hz223awivdp5qzk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.13)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-headers-core" ,rust-headers-core-0.2)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-httpdate" ,rust-httpdate-1)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-sha1" ,rust-sha1-0.10))))
    (home-page "https://hyper.rs")
    (synopsis "typed HTTP headers")
    (description "typed HTTP headers")
    (license license:expat)))
(define-public rust-axum-macros-0.2
  (package
    (name "rust-axum-macros")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "axum-macros" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1f182rj15717707r74lf4ppqca2kz1y8avkklsfng3khxkidm4v2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-heck" ,rust-heck-0.4)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/tokio-rs/axum")
    (synopsis "Macros for axum")
    (description "Macros for axum")
    (license license:expat)))
(define-public rust-http-body-0.4
  (package
    (name "rust-http-body")
    (version "0.4.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "http-body" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1l967qwwlvhp198xdrnc0p5d7jwfcp6q2lm510j6zqw4s4b8zwym"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2))))
    (home-page "https://github.com/hyperium/http-body")
    (synopsis
     "Trait representing an asynchronous, streaming, HTTP request or response body.
")
    (description
     "Trait representing an asynchronous, streaming, HTTP request or response body.")
    (license license:expat)))
(define-public rust-axum-core-0.2
  (package
    (name "rust-axum-core")
    (version "0.2.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "axum-core" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "16w3ccckcbfnbdlbffbmjbgjx9s2a1iiymrdv29s5wh602kc1w6r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-http-body" ,rust-http-body-0.4)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3))))
    (home-page "https://github.com/tokio-rs/axum")
    (synopsis "Core types and traits for axum")
    (description "Core types and traits for axum")
    (license license:expat)))
(define-public rust-axum-0.5
  (package
    (name "rust-axum")
    (version "0.5.16")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "axum" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0hqhnfx55lf9hsis6l3g4nz20a2azg7s53bv8vbadmn48il3bqy9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-axum-core" ,rust-axum-core-0.2)
                       ("rust-axum-macros" ,rust-axum-macros-0.2)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-headers" ,rust-headers-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-http-body" ,rust-http-body-0.4)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-matchit" ,rust-matchit-0.5)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-multer" ,rust-multer-2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
                       ("rust-sha-1" ,rust-sha-1-0.10)
                       ("rust-sync-wrapper" ,rust-sync-wrapper-0.1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-tungstenite" ,rust-tokio-tungstenite-0.17)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-tower-http" ,rust-tower-http-0.3)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3))))
    (home-page "https://github.com/tokio-rs/axum")
    (synopsis "Web framework that focuses on ergonomics and modularity")
    (description "Web framework that focuses on ergonomics and modularity")
    (license license:expat)))
(define-public rust-version-check-0.9
  (package
    (name "rust-version-check")
    (version "0.9.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "version_check" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gs8grwdlgh0xq660d7wr80x14vxbizmd8dbp29p2pdncx8lp1s9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/SergioBenitez/version_check")
    (synopsis
     "Tiny crate to check the version of the installed/running rustc.")
    (description
     "Tiny crate to check the version of the installed/running rustc.")
    (license (list license:expat license:asl2.0))))
(define-public rust-universal-hash-0.5
  (package
    (name "rust-universal-hash")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "universal-hash" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1dfqh2jnf4pz2cr9v4adpyxinz658vadlbwsjgigf6cs7jvn0cbx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-crypto-common" ,rust-crypto-common-0.1)
                       ("rust-subtle" ,rust-subtle-2))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis
     "Traits which describe the functionality of universal hash functions (UHFs)")
    (description
     "Traits which describe the functionality of universal hash functions (UHFs)")
    (license (list license:expat license:asl2.0))))
(define-public rust-polyval-0.6
  (package
    (name "rust-polyval")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "polyval" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1iihmpn1h1ag5zl368yfq0jz1drfdw7xg7zpaqpcppqiikh39wky"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cpufeatures" ,rust-cpufeatures-0.2)
                       ("rust-opaque-debug" ,rust-opaque-debug-0.3)
                       ("rust-universal-hash" ,rust-universal-hash-0.5)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/universal-hashes")
    (synopsis
     "POLYVAL is a GHASH-like universal hash over GF(2^128) useful for constructing
a Message Authentication Code (MAC)
")
    (description
     "POLYVAL is a GHASH-like universal hash over GF(2^128) useful for constructing a
Message Authentication Code (MAC)")
    (license (list license:asl2.0 license:expat))))
(define-public rust-ghash-0.5
  (package
    (name "rust-ghash")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ghash" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0h1y3v3kj8xxkf2snv1yly0lr20fdh3jrm60p382szbiwl6pac6r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-opaque-debug" ,rust-opaque-debug-0.3)
                       ("rust-polyval" ,rust-polyval-0.6)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/universal-hashes")
    (synopsis
     "Universal hash over GF(2^128) useful for constructing a Message Authentication Code (MAC),
as in the AES-GCM authenticated encryption cipher.
")
    (description
     "Universal hash over GF(2^128) useful for constructing a Message Authentication
Code (MAC), as in the AES-GCM authenticated encryption cipher.")
    (license (list license:asl2.0 license:expat))))
(define-public rust-ctr-0.9
  (package
    (name "rust-ctr")
    (version "0.9.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ctr" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0d88b73waamgpfjdml78icxz45d95q7vi2aqa604b0visqdfws83"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cipher" ,rust-cipher-0.4))))
    (home-page "https://github.com/RustCrypto/block-modes")
    (synopsis "CTR block modes of operation")
    (description "CTR block modes of operation")
    (license (list license:expat license:asl2.0))))
(define-public rust-block-padding-0.3
  (package
    (name "rust-block-padding")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "block-padding" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0y5v92alqzn9ikmyqfl3a4j6va87j967ii2n3jh2h330z4nyr40a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-generic-array" ,rust-generic-array-0.14))))
    (home-page "https://github.com/RustCrypto/utils")
    (synopsis "Padding and unpadding of messages divided into blocks.")
    (description "Padding and unpadding of messages divided into blocks.")
    (license (list license:expat license:asl2.0))))
(define-public rust-inout-0.1
  (package
    (name "rust-inout")
    (version "0.1.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "inout" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1xf9gf09nc7y1a261xlfqsf66yn6mb81ahlzzyyd1934sr9hbhd0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-block-padding" ,rust-block-padding-0.3)
                       ("rust-generic-array" ,rust-generic-array-0.14))))
    (home-page "https://github.com/RustCrypto/utils")
    (synopsis
     "Custom reference types for code generic over in-place and buffer-to-buffer modes of operation.")
    (description
     "Custom reference types for code generic over in-place and buffer-to-buffer modes
of operation.")
    (license (list license:expat license:asl2.0))))
(define-public rust-cipher-0.4
  (package
    (name "rust-cipher")
    (version "0.4.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cipher" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "17mmmqaalirdx7bpdhrgzp1sd392zm08mjrr24cjr57pz1q351yi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-blobby" ,rust-blobby-0.3)
                       ("rust-crypto-common" ,rust-crypto-common-0.1)
                       ("rust-inout" ,rust-inout-0.1)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis "Traits for describing block ciphers and stream ciphers")
    (description "Traits for describing block ciphers and stream ciphers")
    (license (list license:expat license:asl2.0))))
(define-public rust-aes-0.8
  (package
    (name "rust-aes")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "aes" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1fj03mqa45nf2scxcd7mvg1xcbavrkqlmkfzwcgnx660g0si7q5z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cipher" ,rust-cipher-0.4)
                       ("rust-cpufeatures" ,rust-cpufeatures-0.2))))
    (home-page "https://github.com/RustCrypto/block-ciphers")
    (synopsis
     "Pure Rust implementation of the Advanced Encryption Standard (a.k.a. Rijndael)")
    (description
     "Pure Rust implementation of the Advanced Encryption Standard (a.k.a.  Rijndael)")
    (license (list license:expat license:asl2.0))))
(define-public rust-aead-0.5
  (package
    (name "rust-aead")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "aead" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1j6pmc8pk4ha64bj9l6xzbhd85s2y1dblna2zsq83h0zy6w2w6aw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-blobby" ,rust-blobby-0.3)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-crypto-common" ,rust-crypto-common-0.1)
                       ("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-heapless" ,rust-heapless-0.7))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis
     "Traits for Authenticated Encryption with Associated Data (AEAD) algorithms,
such as AES-GCM as ChaCha20Poly1305, which provide a high-level API
")
    (description
     "Traits for Authenticated Encryption with Associated Data (AEAD) algorithms, such
as AES-GCM as ChaCha20Poly1305, which provide a high-level API")
    (license (list license:expat license:asl2.0))))
(define-public rust-aes-gcm-0.10
  (package
    (name "rust-aes-gcm")
    (version "0.10.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "aes-gcm" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0z2429v2d2wyf809h2wc4vwwibwypz3y4p7sn4kzkjb91ip3dqc2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aead" ,rust-aead-0.5)
                       ("rust-aes" ,rust-aes-0.8)
                       ("rust-cipher" ,rust-cipher-0.4)
                       ("rust-ctr" ,rust-ctr-0.9)
                       ("rust-ghash" ,rust-ghash-0.5)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/AEADs")
    (synopsis "Pure Rust implementation of the AES-GCM (Galois/Counter Mode)
Authenticated Encryption with Associated Data (AEAD) Cipher
with optional architecture-specific hardware acceleration
")
    (description
     "Pure Rust implementation of the AES-GCM (Galois/Counter Mode) Authenticated
Encryption with Associated Data (AEAD) Cipher with optional
architecture-specific hardware acceleration")
    (license (list license:asl2.0 license:expat))))
(define-public rust-cookie-0.16
  (package
    (name "rust-cookie")
    (version "0.16.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cookie" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "05s9mgrwvbr08f2h57670q9g5z4jjm8zxi5i7hlk5vrr28vxqjil"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aes-gcm" ,rust-aes-gcm-0.10)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-hkdf" ,rust-hkdf-0.12)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-version-check" ,rust-version-check-0.9))))
    (home-page "https://github.com/SergioBenitez/cookie-rs")
    (synopsis
     "HTTP cookie parsing and cookie jar management. Supports signed and private
(encrypted, authenticated) jars.
")
    (description
     "HTTP cookie parsing and cookie jar management.  Supports signed and private
(encrypted, authenticated) jars.")
    (license (list license:expat license:asl2.0))))
(define-public rust-actix-web-codegen-4
  (package
    (name "rust-actix-web-codegen")
    (version "4.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "actix-web-codegen" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "04vbqwbiy4pa1c5d17m2afhcicjcksjgpsnmn9kksr68cck3da8z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-actix-router" ,rust-actix-router-0.5)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://actix.rs")
    (synopsis "Routing and runtime macros for Actix Web")
    (description "Routing and runtime macros for Actix Web")
    (license (list license:expat license:asl2.0))))
(define-public rust-actix-server-2
  (package
    (name "rust-actix-server")
    (version "2.1.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "actix-server" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "096q1hj88kgzfk6zv251sn57dlswh65r8ds6pdvv18cycn74z8qd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-actix-rt" ,rust-actix-rt-2)
                       ("rust-actix-service" ,rust-actix-service-2)
                       ("rust-actix-utils" ,rust-actix-utils-3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-mio" ,rust-mio-0.8)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-socket2" ,rust-socket2-0.4)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-uring" ,rust-tokio-uring-0.3)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://actix.rs")
    (synopsis "General purpose TCP server built for the Actix ecosystem")
    (description "General purpose TCP server built for the Actix ecosystem")
    (license (list license:expat license:asl2.0))))
(define-public rust-actix-router-0.5
  (package
    (name "rust-actix-router")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "actix-router" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "16c7lcis96plz0rl23l44wsq61jpx1bn91m23y361cfj8z9g8vyn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytestring" ,rust-bytestring-0.1)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/actix/actix-web.git")
    (synopsis "Resource path matching and router")
    (description "Resource path matching and router")
    (license (list license:expat license:asl2.0))))
(define-public rust-zstd-sys-2
  (package
    (name "rust-zstd-sys")
    (version "2.0.1+zstd.1.5.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zstd-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0azifd7xsyy9yljihx26pr9am85717fzdzdzbladjiiqqnxprl4z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.59)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/gyscos/zstd-rs")
    (synopsis "Low-level bindings for the zstd compression library.")
    (description "Low-level bindings for the zstd compression library.")
    (license (list license:expat license:asl2.0))))
(define-public rust-zstd-safe-5
  (package
    (name "rust-zstd-safe")
    (version "5.0.2+zstd.1.5.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zstd-safe" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1nzl4q3xl68pq58g9xlym299bvjdii8cl7ix595ym7jgw22maahx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-zstd-sys" ,rust-zstd-sys-2))))
    (home-page "https://github.com/gyscos/zstd-rs")
    (synopsis "Safe low-level bindings for the zstd compression library.")
    (description "Safe low-level bindings for the zstd compression library.")
    (license (list license:expat license:asl2.0))))
(define-public rust-zstd-0.11
  (package
    (name "rust-zstd")
    (version "0.11.2+zstd.1.5.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zstd" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1r7xlmgnifhxbfyid8vkcnd5ip16gx9hf89d1l0lzrpc4q1rdk10"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-zstd-safe" ,rust-zstd-safe-5))))
    (home-page "https://github.com/gyscos/zstd-rs")
    (synopsis "Binding for the zstd compression library.")
    (description "Binding for the zstd compression library.")
    (license license:expat)))
(define-public rust-sha1-0.10
  (package
    (name "rust-sha1")
    (version "0.10.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sha1" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "18zb80sxn31kxdpl1ly6w17hkrvyf08zbxnpy8ckb6f3h3f96hph"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cpufeatures" ,rust-cpufeatures-0.2)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-sha1-asm" ,rust-sha1-asm-0.5))))
    (home-page "https://github.com/RustCrypto/hashes")
    (synopsis "SHA-1 hash function")
    (description "SHA-1 hash function")
    (license (list license:expat license:asl2.0))))
(define-public rust-local-channel-0.1
  (package
    (name "rust-local-channel")
    (version "0.1.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "local-channel" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "177wqgadrlw6m7r6xxafkj58asgpgbpv1ww4gx258v2cx703wc3z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-local-waker" ,rust-local-waker-0.1))))
    (home-page "https://github.com/actix/actix-net.git")
    (synopsis
     "A non-threadsafe multi-producer, single-consumer, futures-aware, FIFO queue")
    (description
     "This package provides a non-threadsafe multi-producer, single-consumer,
futures-aware, FIFO queue")
    (license (list license:expat license:asl2.0))))
(define-public rust-h2-0.3
  (package
    (name "rust-h2")
    (version "0.3.14")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "h2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1gihmz4bwrh9xmr6222dqhi90gcvdizxhp42n757rb11ry92b8sw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-fnv" ,rust-fnv-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/hyperium/h2")
    (synopsis "An HTTP/2 client and server")
    (description "An HTTP/2 client and server")
    (license license:expat)))
(define-public rust-bytestring-1
  (package
    (name "rust-bytestring")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "bytestring" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "12ljqf7v6x094n89vy6s1ab130dy2abxfp5g0vphi204sdgsgdl6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://actix.rs")
    (synopsis "An immutable UTF-8 encoded string using Bytes as storage")
    (description "An immutable UTF-8 encoded string using Bytes as storage")
    (license (list license:expat license:asl2.0))))
(define-public rust-brotli-3
  (package
    (name "rust-brotli")
    (version "3.3.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "brotli" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0s7z0nrv04wxniwijh5iig1w31sphc6lz38zc8lr7qlarkdv3851"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-alloc-no-stdlib" ,rust-alloc-no-stdlib-2)
                       ("rust-alloc-stdlib" ,rust-alloc-stdlib-0.2)
                       ("rust-brotli-decompressor" ,rust-brotli-decompressor-2)
                       ("rust-packed-simd-2" ,rust-packed-simd-2-0.3)
                       ("rust-sha2" ,rust-sha2-0.8))))
    (home-page "https://github.com/dropbox/rust-brotli")
    (synopsis
     "A brotli compressor and decompressor that with an interface avoiding the rust stdlib. This makes it suitable for embedded devices and kernels. It is designed with a pluggable allocator so that the standard lib's allocator may be employed. The default build also includes a stdlib allocator and stream interface. Disable this with --features=no-stdlib. All included code is safe.")
    (description
     "This package provides a brotli compressor and decompressor that with an
interface avoiding the rust stdlib.  This makes it suitable for embedded devices
and kernels.  It is designed with a pluggable allocator so that the standard
lib's allocator may be employed.  The default build also includes a stdlib
allocator and stream interface.  Disable this with --features=no-stdlib.  All
included code is safe.")
    (license (list license:bsd-3 license:expat))))
(define-public rust-tokio-rustls-0.23
  (package
    (name "rust-tokio-rustls")
    (version "0.23.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio-rustls" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0nfsmmi8l1lgpbfy6079d5i13984djzcxrdr9jc06ghi0cwyhgn4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustls" ,rust-rustls-0.20)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-webpki" ,rust-webpki-0.22))))
    (home-page "https://github.com/tokio-rs/tls")
    (synopsis "Asynchronous TLS/SSL streams for Tokio using Rustls.")
    (description "Asynchronous TLS/SSL streams for Tokio using Rustls.")
    (license (list license:expat license:asl2.0))))
(define-public rust-http-0.2
  (package
    (name "rust-http")
    (version "0.2.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "http" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1693pkg43czk26fima0l0l5h2h9rvm8n84pff5zc35b9w90kvx3m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-fnv" ,rust-fnv-1)
                       ("rust-itoa" ,rust-itoa-1))))
    (home-page "https://github.com/hyperium/http")
    (synopsis "A set of types for representing HTTP requests and responses.
")
    (description
     "This package provides a set of types for representing HTTP requests and
responses.")
    (license (list license:expat license:asl2.0))))
(define-public rust-local-waker-0.1
  (package
    (name "rust-local-waker")
    (version "0.1.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "local-waker" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1w9zqlh18mymvb82ya0sailiy5d3wsjamaakgl70x50i6vmpckz3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/actix/actix-net.git")
    (synopsis "A synchronization primitive for thread-local task wakeup")
    (description
     "This package provides a synchronization primitive for thread-local task wakeup")
    (license (list license:expat license:asl2.0))))
(define-public rust-actix-utils-3
  (package
    (name "rust-actix-utils")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "actix-utils" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "155aj87z8634mfmggfixyqy3pqhpyf7g97zrzy6piz77qamcp4g4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-local-waker" ,rust-local-waker-0.1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2))))
    (home-page "https://github.com/actix/actix-net")
    (synopsis "Various utilities used in the Actix ecosystem")
    (description "Various utilities used in the Actix ecosystem")
    (license (list license:expat license:asl2.0))))
(define-public rust-actix-tls-3
  (package
    (name "rust-actix-tls")
    (version "3.0.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "actix-tls" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "15rj6gn83fzv5w2b2y0s690q80awsjhbjg40f3qcgkgpjbr0rplz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-actix-codec" ,rust-actix-codec-0.5)
                       ("rust-actix-rt" ,rust-actix-rt-2)
                       ("rust-actix-service" ,rust-actix-service-2)
                       ("rust-actix-utils" ,rust-actix-utils-3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-openssl" ,rust-tokio-openssl-0.6)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.23)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.22))))
    (home-page "https://github.com/actix/actix-net.git")
    (synopsis "TLS acceptor and connector services for Actix ecosystem")
    (description "TLS acceptor and connector services for Actix ecosystem")
    (license (list license:expat license:asl2.0))))
(define-public rust-actix-service-2
  (package
    (name "rust-actix-service")
    (version "2.0.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "actix-service" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0fipjcc5kma7j47jfrw55qm09dakgvx617jbriydrkqqz10lk29v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-paste" ,rust-paste-1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2))))
    (home-page "https://github.com/actix/actix-net")
    (synopsis
     "Service trait and combinators for representing asynchronous request/response operations.")
    (description
     "Service trait and combinators for representing asynchronous request/response
operations.")
    (license (list license:expat license:asl2.0))))
(define-public rust-sc-0.2
  (package
    (name "rust-sc")
    (version "0.2.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "12x3c3mn36am3jfamswqfsd0vpr0hz3kdck6wskla7gx7fyih3h1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/japaric/syscall.rs")
    (synopsis "Raw system calls")
    (description "Raw system calls")
    (license (list license:expat license:asl2.0))))
(define-public rust-bindgen-0.60
  (package
    (name "rust-bindgen")
    (version "0.60.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "bindgen" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1rl8pzzbxsgkx0v20bvvbwrlqhbifzw2p3ikwrns9b543fydsb86"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-cexpr" ,rust-cexpr-0.6)
                       ("rust-clang-sys" ,rust-clang-sys-1)
                       ("rust-clap" ,rust-clap-3)
                       ("rust-env-logger" ,rust-env-logger-0.9)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-lazycell" ,rust-lazycell-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-peeking-take-while" ,rust-peeking-take-while-0.1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-rustc-hash" ,rust-rustc-hash-1)
                       ("rust-shlex" ,rust-shlex-1)
                       ("rust-which" ,rust-which-4))))
    (home-page "https://rust-lang.github.io/rust-bindgen/")
    (synopsis
     "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (description
     "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (license license:bsd-3)))
(define-public rust-io-uring-0.5
  (package
    (name "rust-io-uring")
    (version "0.5.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "io-uring" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "02qjf26x16v0f9yvqhz06yjas97nx16hsz8g0ma2z304wzc5yr2s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.60)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-sc" ,rust-sc-0.2))))
    (home-page "https://github.com/tokio-rs/io-uring")
    (synopsis "The low-level `io_uring` userspace interface for Rust")
    (description "The low-level `io_uring` userspace interface for Rust")
    (license (list license:expat license:asl2.0))))
(define-public rust-tokio-uring-0.3
  (package
    (name "rust-tokio-uring")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio-uring" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1j22wyryp8ami8gq9cgh3wqd7g5gklqzdrxdj3cq8jc7757lkbfk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-io-uring" ,rust-io-uring-0.5)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-scoped-tls" ,rust-scoped-tls-1)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-socket2" ,rust-socket2-0.4)
                       ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://tokio.rs")
    (synopsis "io-uring support for the Tokio asynchronous runtime.
")
    (description "io-uring support for the Tokio asynchronous runtime.")
    (license license:expat)))
(define-public rust-actix-macros-0.2
  (package
    (name "rust-actix-macros")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "actix-macros" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1dhk2bdp6rj67j5zgi4b76hpy2xw567js0hig28n1fb9rxr62nj6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/actix/actix-net.git")
    (synopsis "Macros for Actix system and runtime")
    (description "Macros for Actix system and runtime")
    (license (list license:expat license:asl2.0))))
(define-public rust-actix-rt-2
  (package
    (name "rust-actix-rt")
    (version "2.7.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "actix-rt" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "000hxsbaxgd8jdmnw4dnlff4xdhggprnw2lk67pmiscqa4lnr8by"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-actix-macros" ,rust-actix-macros-0.2)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-uring" ,rust-tokio-uring-0.3))))
    (home-page "https://actix.rs")
    (synopsis
     "Tokio-based single-threaded async runtime for the Actix ecosystem")
    (description
     "Tokio-based single-threaded async runtime for the Actix ecosystem")
    (license (list license:expat license:asl2.0))))
(define-public rust-actix-http-3
  (package
    (name "rust-actix-http")
    (version "3.2.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "actix-http" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0917rn3pkq1rlfvsal9zb0xa1mbzfswzgi1wjzchl7ryj3wsp0qc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-actix-codec" ,rust-actix-codec-0.5)
                       ("rust-actix-rt" ,rust-actix-rt-2)
                       ("rust-actix-service" ,rust-actix-service-2)
                       ("rust-actix-tls" ,rust-actix-tls-3)
                       ("rust-actix-utils" ,rust-actix-utils-3)
                       ("rust-ahash" ,rust-ahash-0.7)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-brotli" ,rust-brotli-3)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-bytestring" ,rust-bytestring-1)
                       ("rust-derive-more" ,rust-derive-more-0.99)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-h2" ,rust-h2-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-httpdate" ,rust-httpdate-1)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-language-tags" ,rust-language-tags-0.3)
                       ("rust-local-channel" ,rust-local-channel-0.1)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-zstd" ,rust-zstd-0.11))))
    (home-page "https://actix.rs")
    (synopsis "HTTP primitives for the Actix ecosystem")
    (description "HTTP primitives for the Actix ecosystem")
    (license (list license:expat license:asl2.0))))
(define-public rust-socket2-0.4
  (package
    (name "rust-socket2")
    (version "0.4.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "socket2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1gaf57dc16s1lfyv388w9vdl9qay15xds78jcwakml9kj3dx5qh2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/rust-lang/socket2")
    (synopsis
     "Utilities for handling networking sockets with a maximal amount of configuration
possible intended.
")
    (description
     "Utilities for handling networking sockets with a maximal amount of configuration
possible intended.")
    (license (list license:expat license:asl2.0))))
(define-public rust-wasi-0.11
  (package
    (name "rust-wasi")
    (version "0.11.0+wasi-snapshot-preview1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wasi" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "08z4hxwkpdpalxjps1ai9y7ihin26y9f476i53dv98v45gkqg3cw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/bytecodealliance/wasi")
    (synopsis "Experimental WASI API bindings for Rust")
    (description "Experimental WASI API bindings for Rust")
    (license (list license:asl2.0 license:asl2.0 license:expat))))
(define-public rust-libc-0.2
  (package
    (name "rust-libc")
    (version "0.2.134")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ywzv7fpqlbpbfd3i1210rhd5r4bkv46ivkmsngw8svk90sr771j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/rust-lang/libc")
    (synopsis "Raw FFI bindings to platform libraries like libc.
")
    (description "Raw FFI bindings to platform libraries like libc.")
    (license (list license:expat license:asl2.0))))
(define-public rust-mio-0.8
  (package
    (name "rust-mio")
    (version "0.8.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "mio" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1byahxxpnm42djgip44b545i2pr5d7fgyff3a290qfy6qwiirvjp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-wasi" ,rust-wasi-0.11)
                       ("rust-windows-sys" ,rust-windows-sys-0.36))))
    (home-page "https://github.com/tokio-rs/mio")
    (synopsis "Lightweight non-blocking IO")
    (description "Lightweight non-blocking IO")
    (license license:expat)))
(define-public rust-tokio-1
  (package
    (name "rust-tokio")
    (version "1.21.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "16chkl1wabwinnqya4zrjz7a1wn6mb20s699lwmp0mf9gm4krq59"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-autocfg" ,rust-autocfg-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-mio" ,rust-mio-0.8)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-signal-hook-registry" ,rust-signal-hook-registry-1)
                       ("rust-socket2" ,rust-socket2-0.4)
                       ("rust-tokio-macros" ,rust-tokio-macros-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://tokio.rs")
    (synopsis
     "An event-driven, non-blocking I/O platform for writing asynchronous I/O
backed applications.
")
    (description
     "An event-driven, non-blocking I/O platform for writing asynchronous I/O backed
applications.")
    (license license:expat)))
(define-public rust-slab-0.4
  (package
    (name "rust-slab")
    (version "0.4.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "slab" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1vyw3rkdfdfkzfa1mh83s237sll8v5kazfwxma60bq4b59msf526"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-autocfg" ,rust-autocfg-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/tokio-rs/slab")
    (synopsis "Pre-allocated storage for a uniform data type")
    (description "Pre-allocated storage for a uniform data type")
    (license license:expat)))
(define-public rust-tokio-util-0.7
  (package
    (name "rust-tokio-util")
    (version "0.7.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio-util" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0h67jb56bsxy4pi1a41pda8d52569ci5clvqv3c6cg9vy1sy1chb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-hashbrown" ,rust-hashbrown-0.12)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://tokio.rs")
    (synopsis "Additional utilities for working with Tokio.
")
    (description "Additional utilities for working with Tokio.")
    (license license:expat)))
(define-public rust-actix-codec-0.5
  (package
    (name "rust-actix-codec")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "actix-codec" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1zm7nk8irjgkf08a6x632niwd9iprq43rdda4wqmgwx70ja5b9sp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7))))
    (home-page "https://github.com/actix/actix-net")
    (synopsis "Codec utilities for working with framed protocols")
    (description "Codec utilities for working with framed protocols")
    (license (list license:expat license:asl2.0))))
(define-public rust-actix-web-4
  (package
    (name "rust-actix-web")
    (version "4.2.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "actix-web" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1fcn3srgpzv83ssj7lp74gz6mbyl2xwxp4gffby7nv706ijpp3yl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-actix-codec" ,rust-actix-codec-0.5)
                       ("rust-actix-http" ,rust-actix-http-3)
                       ("rust-actix-macros" ,rust-actix-macros-0.2)
                       ("rust-actix-router" ,rust-actix-router-0.5)
                       ("rust-actix-rt" ,rust-actix-rt-2)
                       ("rust-actix-server" ,rust-actix-server-2)
                       ("rust-actix-service" ,rust-actix-service-2)
                       ("rust-actix-tls" ,rust-actix-tls-3)
                       ("rust-actix-utils" ,rust-actix-utils-3)
                       ("rust-actix-web-codegen" ,rust-actix-web-codegen-4)
                       ("rust-ahash" ,rust-ahash-0.7)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-bytestring" ,rust-bytestring-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cookie" ,rust-cookie-0.16)
                       ("rust-derive-more" ,rust-derive-more-0.99)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-language-tags" ,rust-language-tags-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-socket2" ,rust-socket2-0.4)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://actix.rs")
    (synopsis
     "Actix Web is a powerful, pragmatic, and extremely fast web framework for Rust")
    (description
     "Actix Web is a powerful, pragmatic, and extremely fast web framework for Rust")
    (license (list license:expat license:asl2.0))))
(define-public rust-rust-embed-6
  (package
    (name "rust-rust-embed")
    (version "6.4.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rust-embed" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ymmmw010fp1rjgn1nhrc194l6yc8x0bl776xxji3nm1cz6k8sg2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-actix-web" ,rust-actix-web-4)
                       ("rust-axum" ,rust-axum-0.5)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-include-flate" ,rust-include-flate-0.1)
                       ("rust-mime-guess" ,rust-mime-guess-2)
                       ("rust-poem" ,rust-poem-1)
                       ("rust-rocket" ,rust-rocket-0.5)
                       ("rust-rust-embed-impl" ,rust-rust-embed-impl-6)
                       ("rust-rust-embed-utils" ,rust-rust-embed-utils-7)
                       ("rust-salvo" ,rust-salvo-0.16)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-walkdir" ,rust-walkdir-2)
                       ("rust-warp" ,rust-warp-0.3))))
    (home-page "https://github.com/pyros2097/rust-embed")
    (synopsis
     "Rust Custom Derive Macro which loads files into the rust binary at compile time during release and loads the file from the fs during dev")
    (description
     "Rust Custom Derive Macro which loads files into the rust binary at compile time
during release and loads the file from the fs during dev")
    (license license:expat)))
(define-public rust-autocfg-1
  (package
    (name "rust-autocfg")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "autocfg" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ylp3cb47ylzabimazvbz9ms6ap784zhb6syaz6c1jqpmcmq0s6l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/cuviper/autocfg")
    (synopsis "Automatic cfg for Rust compiler features")
    (description "Automatic cfg for Rust compiler features")
    (license (list license:asl2.0 license:expat))))
(define-public rust-lock-api-0.4
  (package
    (name "rust-lock-api")
    (version "0.4.9")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "lock_api" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1py41vk243hwk345nhkn5nw0bd4m03gzjmprdjqq6rg5dwv12l23"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-autocfg" ,rust-autocfg-1)
                       ("rust-owning-ref" ,rust-owning-ref-0.4)
                       ("rust-scopeguard" ,rust-scopeguard-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/Amanieu/parking_lot")
    (synopsis
     "Wrappers to create fully-featured Mutex and RwLock types. Compatible with no_std.")
    (description
     "Wrappers to create fully-featured Mutex and RwLock types.  Compatible with
no_std.")
    (license (list license:expat license:asl2.0))))
(define-public rust-parking-lot-0.12
  (package
    (name "rust-parking-lot")
    (version "0.12.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "parking_lot" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "13r2xk7mnxfc5g0g6dkdxqdqad99j7s7z8zhzz4npw5r0g0v4hip"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lock-api" ,rust-lock-api-0.4)
                       ("rust-parking-lot-core" ,rust-parking-lot-core-0.9))))
    (home-page "https://github.com/Amanieu/parking_lot")
    (synopsis
     "More compact and efficient implementations of the standard synchronization primitives.")
    (description
     "More compact and efficient implementations of the standard synchronization
primitives.")
    (license (list license:expat license:asl2.0))))
(define-public rust-i18n-config-0.4
  (package
    (name "rust-i18n-config")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "i18n-config" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "10yr0k8vy2s60yzajn659yscb3ch0ya6ywyqrgrm3yxb8g6zyamn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-toml" ,rust-toml-0.5)
                       ("rust-unic-langid" ,rust-unic-langid-0.9))))
    (home-page
     "https://github.com/kellpossible/cargo-i18n/tree/master/i18n-config")
    (synopsis
     "This library contains the configuration stucts (along with their parsing functions) for the cargo-i18n tool/system.")
    (description
     "This library contains the configuration stucts (along with their parsing
functions) for the cargo-i18n tool/system.")
    (license license:expat)))
(define-public rust-find-crate-0.6
  (package
    (name "rust-find-crate")
    (version "0.6.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "find-crate" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ljpkh11gj7940xwz47xjhsvfbl93c2q0ql7l2v0w77amjx8paar"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-toml" ,rust-toml-0.5))))
    (home-page "https://github.com/taiki-e/find-crate")
    (synopsis "Find the crate name from the current Cargo.toml.
")
    (description "Find the crate name from the current Cargo.toml.")
    (license (list license:asl2.0 license:expat))))
(define-public rust-i18n-embed-impl-0.8
  (package
    (name "rust-i18n-embed-impl")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "i18n-embed-impl" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0dk1ww6f0xi4zjyz5q7k79v57qyw7msfcrzv983fn22q0c737chd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-find-crate" ,rust-find-crate-0.6)
                       ("rust-i18n-config" ,rust-i18n-config-0.4)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page
     "https://github.com/kellpossible/cargo-i18n/tree/master/i18n-embed")
    (synopsis "Macro implementations for i18n-embed")
    (description "Macro implementations for i18n-embed")
    (license license:expat)))
(define-public rust-gettext-0.4
  (package
    (name "rust-gettext")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "gettext" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0wd9kfy7nmbrqx2znw186la99as8y265lvh3pvj9fn9xfm75kfwy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-encoding" ,rust-encoding-0.2))))
    (home-page "https://github.com/justinas/gettext")
    (synopsis "An implementation of Gettext translation framework for Rust")
    (description "An implementation of Gettext translation framework for Rust")
    (license license:expat)))
(define-public rust-fluent-pseudo-0.3
  (package
    (name "rust-fluent-pseudo")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "fluent-pseudo" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0byldssmzjdmynbh1yvdrxcj0xmhqznlmmgwnh8a1fhla7wn5vgx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-regex" ,rust-regex-1))))
    (home-page "http://www.projectfluent.org")
    (synopsis
     "Pseudolocalization transformation API for use with Project Fluent API.
")
    (description
     "Pseudolocalization transformation API for use with Project Fluent API.")
    (license (list license:asl2.0 license:expat))))
(define-public rust-self-cell-0.10
  (package
    (name "rust-self-cell")
    (version "0.10.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "self_cell" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1by8h3axgpbiph5nbq80z6a41hd4cqlqc66hgnngs57y42j6by8y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustversion" ,rust-rustversion-1))))
    (home-page "https://github.com/Voultapher/self_cell")
    (synopsis
     "Safe-to-use proc-macro-free self-referential structs in stable Rust.")
    (description
     "Safe-to-use proc-macro-free self-referential structs in stable Rust.")
    (license license:asl2.0)))
(define-public rust-intl-pluralrules-7
  (package
    (name "rust-intl-pluralrules")
    (version "7.0.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "intl_pluralrules" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ksy3hxqs8if3nbvcin0a8390lpkzbk2br1brik70z96hj1ri3xi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-tinystr" ,rust-tinystr-0.3)
                       ("rust-unic-langid" ,rust-unic-langid-0.9))))
    (home-page "https://github.com/zbraniecki/pluralrules")
    (synopsis "Unicode Plural Rules categorizer for numeric input.")
    (description "Unicode Plural Rules categorizer for numeric input.")
    (license (list license:asl2.0 license:expat))))
(define-public rust-type-map-0.4
  (package
    (name "rust-type-map")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "type-map" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ilsqq7pcl3k9ggxv2x5fbxxfd6x7ljsndrhc38jmjwnbr63dlxn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustc-hash" ,rust-rustc-hash-1))))
    (home-page "https://github.com/kardeiz/type-map")
    (synopsis "Provides a typemap container with FxHashMap")
    (description "This package provides a typemap container with FxHashMap")
    (license (list license:expat license:asl2.0))))
(define-public rust-intl-memoizer-0.5
  (package
    (name "rust-intl-memoizer")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "intl-memoizer" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0vx6cji8ifw77zrgipwmvy1i3v43dcm58hwjxpb1h29i98z46463"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-type-map" ,rust-type-map-0.4)
                       ("rust-unic-langid" ,rust-unic-langid-0.9))))
    (home-page "http://www.projectfluent.org")
    (synopsis "A memoizer specifically tailored for storing lazy-initialized
intl formatters.
")
    (description
     "This package provides a memoizer specifically tailored for storing
lazy-initialized intl formatters.")
    (license (list license:asl2.0 license:expat))))
(define-public rust-fluent-syntax-0.11
  (package
    (name "rust-fluent-syntax")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "fluent-syntax" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0y6ac7z7sbv51nsa6km5z8rkjj4nvqk91vlghq1ck5c3cjbyvay0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "http://www.projectfluent.org")
    (synopsis "Parser/Serializer tools for Fluent Syntax. 
")
    (description "Parser/Serializer tools for Fluent Syntax. ")
    (license (list license:asl2.0 license:expat))))
(define-public rust-unic-langid-macros-impl-0.9
  (package
    (name "rust-unic-langid-macros-impl")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "unic-langid-macros-impl" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "17xaknz6ixhg2cp8x174d54hdlv78akb2s0kw31p8xg2jzynyf99"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro-hack" ,rust-proc-macro-hack-0.5)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1)
                       ("rust-unic-langid-impl" ,rust-unic-langid-impl-0.9))))
    (home-page "https://github.com/zbraniecki/unic-locale")
    (synopsis "API for managing Unicode Language Identifiers")
    (description "API for managing Unicode Language Identifiers")
    (license (list license:expat license:asl2.0))))
(define-public rust-unic-langid-macros-0.9
  (package
    (name "rust-unic-langid-macros")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "unic-langid-macros" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1rfn9pvbypvxyr8iyfx6dycafnn9ih9v8r3dhgr0b23yv3b81y8q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro-hack" ,rust-proc-macro-hack-0.5)
                       ("rust-tinystr" ,rust-tinystr-0.3)
                       ("rust-unic-langid-impl" ,rust-unic-langid-impl-0.9)
                       ("rust-unic-langid-macros-impl" ,rust-unic-langid-macros-impl-0.9))))
    (home-page "https://github.com/zbraniecki/unic-locale")
    (synopsis "API for managing Unicode Language Identifiers")
    (description "API for managing Unicode Language Identifiers")
    (license (list license:expat license:asl2.0))))
(define-public rust-tinystr-macros-0.1
  (package
    (name "rust-tinystr-macros")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tinystr-macros" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0c5c361iv5h9brfmhrmz1s7456px5acdd5aqjkazcssfs2playn9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-tinystr" ,rust-tinystr-0.3))))
    (home-page "https://github.com/zbraniecki/tinystr")
    (synopsis "Proc macros for TinyStr.
")
    (description "Proc macros for TinyStr.")
    (license (list license:asl2.0 license:expat))))
(define-public rust-tinystr-0.3
  (package
    (name "rust-tinystr")
    (version "0.3.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tinystr" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1hf74r8qiigddfsxsbkab1pz1hsgi2297azf42k9x39qnknqwwr9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-tinystr-macros" ,rust-tinystr-macros-0.1))))
    (home-page "https://github.com/unicode-org/icu4x")
    (synopsis "A small ASCII-only bounded length string representation.")
    (description
     "This package provides a small ASCII-only bounded length string representation.")
    (license (list license:asl2.0 license:expat))))
(define-public rust-unic-langid-impl-0.9
  (package
    (name "rust-unic-langid-impl")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "unic-langid-impl" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0kck3fpdvqv5nha47xkna3zsr8ik9hpyr5ac830n4j29y3m8wjhs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-tinystr" ,rust-tinystr-0.3))))
    (home-page "https://github.com/zbraniecki/unic-locale")
    (synopsis "API for managing Unicode Language Identifiers")
    (description "API for managing Unicode Language Identifiers")
    (license (list license:expat license:asl2.0))))
(define-public rust-unic-langid-0.9
  (package
    (name "rust-unic-langid")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "unic-langid" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1rcw8llr3a120qad7rlbv4fb19l744ckxwnx37dhn0qafg6qyckk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-unic-langid-impl" ,rust-unic-langid-impl-0.9)
                       ("rust-unic-langid-macros" ,rust-unic-langid-macros-0.9))))
    (home-page "https://github.com/zbraniecki/unic-locale")
    (synopsis "API for managing Unicode Language Identifiers")
    (description "API for managing Unicode Language Identifiers")
    (license (list license:expat license:asl2.0))))
(define-public rust-fluent-langneg-0.13
  (package
    (name "rust-fluent-langneg")
    (version "0.13.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "fluent-langneg" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "152yxplc11vmxkslvmaqak9x86xnavnhdqyhrh38ym37jscd0jic"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-unic-langid" ,rust-unic-langid-0.9))))
    (home-page "http://projectfluent.org/")
    (synopsis "A library for language and locale negotiation.
")
    (description
     "This package provides a library for language and locale negotiation.")
    (license license:asl2.0)))
(define-public rust-fluent-bundle-0.15
  (package
    (name "rust-fluent-bundle")
    (version "0.15.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "fluent-bundle" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1zbzm13rfz7fay7bps7jd4j1pdnlxmdzzfymyq2iawf9vq0wchp2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-fluent-langneg" ,rust-fluent-langneg-0.13)
                       ("rust-fluent-syntax" ,rust-fluent-syntax-0.11)
                       ("rust-intl-memoizer" ,rust-intl-memoizer-0.5)
                       ("rust-intl-pluralrules" ,rust-intl-pluralrules-7)
                       ("rust-rustc-hash" ,rust-rustc-hash-1)
                       ("rust-self-cell" ,rust-self-cell-0.10)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-unic-langid" ,rust-unic-langid-0.9))))
    (home-page "http://www.projectfluent.org")
    (synopsis
     "A localization system designed to unleash the entire expressive power of
natural language translations.
")
    (description
     "This package provides a localization system designed to unleash the entire
expressive power of natural language translations.")
    (license (list license:asl2.0 license:expat))))
(define-public rust-fluent-0.16
  (package
    (name "rust-fluent")
    (version "0.16.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "fluent" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "19s7z0gw95qdsp9hhc00xcy11nwhnx93kknjmdvdnna435w97xk1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-fluent-bundle" ,rust-fluent-bundle-0.15)
                       ("rust-fluent-pseudo" ,rust-fluent-pseudo-0.3)
                       ("rust-unic-langid" ,rust-unic-langid-0.9))))
    (home-page "http://www.projectfluent.org")
    (synopsis
     "A localization system designed to unleash the entire expressive power of
natural language translations.
")
    (description
     "This package provides a localization system designed to unleash the entire
expressive power of natural language translations.")
    (license (list license:asl2.0 license:expat))))
(define-public rust-i18n-embed-0.13
  (package
    (name "rust-i18n-embed")
    (version "0.13.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "i18n-embed" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ly2n8y1zgj9953i5ikmvkh0nj76n9xb6sx3vz1qmpj4dvbixwp7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-fluent" ,rust-fluent-0.16)
                       ("rust-fluent-langneg" ,rust-fluent-langneg-0.13)
                       ("rust-fluent-syntax" ,rust-fluent-syntax-0.11)
                       ("rust-gettext" ,rust-gettext-0.4)
                       ("rust-i18n-embed-impl" ,rust-i18n-embed-impl-0.8)
                       ("rust-intl-memoizer" ,rust-intl-memoizer-0.5)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-locale-config" ,rust-locale-config-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-rust-embed" ,rust-rust-embed-6)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tr" ,rust-tr-0.1)
                       ("rust-unic-langid" ,rust-unic-langid-0.9)
                       ("rust-walkdir" ,rust-walkdir-2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page
     "https://github.com/kellpossible/cargo-i18n/tree/master/i18n-embed")
    (synopsis
     "Traits and macros to conveniently embed localization assets into your application binary or library in order to localize it at runtime.")
    (description
     "Traits and macros to conveniently embed localization assets into your
application binary or library in order to localize it at runtime.")
    (license license:expat)))
(define-public rust-dialoguer-0.9
  (package
    (name "rust-dialoguer")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "dialoguer" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1sxy4nd9kd9wslxnjdjyxgmsg5fil3dnzy63z8f07in09vd9lmv1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-console" ,rust-console-0.15)
                       ("rust-fuzzy-matcher" ,rust-fuzzy-matcher-0.3)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/mitsuhiko/dialoguer")
    (synopsis "A command line prompting library.")
    (description "This package provides a command line prompting library.")
    (license license:expat)))
(define-public rust-bech32-0.8
  (package
    (name "rust-bech32")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "bech32" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "12xzbgh12v3wnv5gc4rnr6vyylyghc9xhxzp9b3ib7v3znxz17yg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/rust-bitcoin/rust-bech32")
    (synopsis "Encodes and decodes the Bech32 format")
    (description "Encodes and decodes the Bech32 format")
    (license license:expat)))

(define-public rust-age-plugin-0.3
  (package
    (name "rust-age-plugin")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "age-plugin" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0srlwbcsn8ad44iiycq1p0px0qn9qnlwz3c5dbhb393aa4jw6vdd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-gumdrop" ,rust-gumdrop-0.8))
       #:cargo-inputs (("rust-age-core" ,rust-age-core-0.8)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-bech32" ,rust-bech32-0.8)
                       ("rust-chrono" ,rust-chrono-0.4))))
    (home-page "https://github.com/str4d/rage")
    (synopsis "[BETA] API for writing age plugins.")
    (description "[BETA] API for writing age plugins.")
    (license (list license:expat license:asl2.0))))

(define-public rust-io-tee-0.1
  (package
    (name "rust-io-tee")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "io_tee" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "013ka85akdcsj9rr92jrkm4jia9s8ihirpqi0ncqc6156kppqgsb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/TheOnlyMrCat/io_tee")
    (synopsis "Tee Read, BufRead, and Seek instances to a writer")
    (description "Tee Read, BufRead, and Seek instances to a writer")
    (license (list license:expat license:asl2.0))))
(define-public rust-typenum-1
  (package
    (name "rust-typenum")
    (version "1.15.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "typenum" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "11yrvz1vd43gqv738yw1v75rzngjbs7iwcgzjy3cq5ywkv2imy6w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-scale-info" ,rust-scale-info-1))))
    (home-page "https://github.com/paholg/typenum")
    (synopsis
     "Typenum is a Rust library for type-level numbers evaluated at
    compile time. It currently supports bits, unsigned integers, and signed
    integers. It also provides a type-level array of type-level numbers, but its
    implementation is incomplete.")
    (description
     "Typenum is a Rust library for type-level numbers evaluated at     compile time.
It currently supports bits, unsigned integers, and signed     integers.  It also
provides a type-level array of type-level numbers, but its     implementation is
incomplete.")
    (license (list license:expat license:asl2.0))))
(define-public rust-crypto-common-0.1
  (package
    (name "rust-crypto-common")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "crypto-common" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1cvby95a6xg7kxdz5ln3rl9xh66nz66w46mm3g56ri1z5x815yqv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-typenum" ,rust-typenum-1))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis "Common cryptographic traits")
    (description "Common cryptographic traits")
    (license (list license:expat license:asl2.0))))
(define-public rust-const-oid-0.9
  (package
    (name "rust-const-oid")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "const-oid" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0q8n1zsa73130hxa2w88qw36g8nprz21j52abpva3khm59a26bkj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/RustCrypto/formats/tree/master/const-oid")
    (synopsis
     "Const-friendly implementation of the ISO/IEC Object Identifier (OID) standard
as defined in ITU X.660, with support for BER/DER encoding/decoding as well as
heapless no_std (i.e. embedded) support
")
    (description
     "Const-friendly implementation of the ISO/IEC Object Identifier (OID) standard as
defined in ITU X.660, with support for BER/DER encoding/decoding as well as
heapless no_std (i.e.  embedded) support")
    (license (list license:asl2.0 license:expat))))
(define-public rust-digest-0.10
  (package
    (name "rust-digest")
    (version "0.10.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "digest" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0v7qvhh0apbgagnj2dc1x8pnwxmvd5z4vdpjxg9cnym3cmrwbyxd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-blobby" ,rust-blobby-0.3)
                       ("rust-block-buffer" ,rust-block-buffer-0.10)
                       ("rust-const-oid" ,rust-const-oid-0.9)
                       ("rust-crypto-common" ,rust-crypto-common-0.1)
                       ("rust-subtle" ,rust-subtle-2))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis "Traits for cryptographic hash functions")
    (description "Traits for cryptographic hash functions")
    (license (list license:expat license:asl2.0))))
(define-public rust-hmac-0.12
  (package
    (name "rust-hmac")
    (version "0.12.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "hmac" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0pmbr069sfg76z7wsssfk5ddcqd9ncp79fyz6zcm6yn115yc6jbc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-digest" ,rust-digest-0.10))))
    (home-page "https://github.com/RustCrypto/MACs")
    (synopsis
     "Generic implementation of Hash-based Message Authentication Code (HMAC)")
    (description
     "Generic implementation of Hash-based Message Authentication Code (HMAC)")
    (license (list license:expat license:asl2.0))))
(define-public rust-hkdf-0.12
  (package
    (name "rust-hkdf")
    (version "0.12.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "hkdf" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0dyl16cf15hka32hv3l7dwgr3xj3brpfr27iyrbpdhlzdfgh46kr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-hmac" ,rust-hmac-0.12))))
    (home-page "https://github.com/RustCrypto/KDFs/")
    (synopsis "HMAC-based Extract-and-Expand Key Derivation Function (HKDF)")
    (description
     "HMAC-based Extract-and-Expand Key Derivation Function (HKDF)")
    (license (list license:expat license:asl2.0))))
(define-public rust-age-core-0.8
  (package
    (name "rust-age-core")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "age-core" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0igsjziq0g4yfbs8hiqq1sfibpaiacqad5iq76np9g1slgcci980"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.13)
                       ("rust-chacha20poly1305" ,rust-chacha20poly1305-0.9)
                       ("rust-cookie-factory" ,rust-cookie-factory-0.3)
                       ("rust-hkdf" ,rust-hkdf-0.12)
                       ("rust-io-tee" ,rust-io-tee-0.1)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-secrecy" ,rust-secrecy-0.8)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/str4d/rage")
    (synopsis "[BETA] Common functions used across the age crates")
    (description "[BETA] Common functions used across the age crates")
    (license (list license:expat license:asl2.0))))

(define-public rust-age-plugin-yubikey-0.3
  (package
    (name "rust-age-plugin-yubikey")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "age-plugin-yubikey" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "141yd51l67sbxv2d7x26p9cscw9kx7ljc1waqfkhln7br8zsb7bv"))))
    (build-system cargo-build-system)
    (native-inputs (list pkg-config))
    (inputs (list pcsc-lite))
    (arguments
     `(#:cargo-inputs (("rust-age-core" ,rust-age-core-0.8)
                       ("rust-age-plugin" ,rust-age-plugin-0.3)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-bech32" ,rust-bech32-0.8)
                       ("rust-console" ,rust-console-0.15)
                       ("rust-dialoguer" ,rust-dialoguer-0.9)
                       ("rust-env-logger" ,rust-env-logger-0.9)
                       ("rust-gumdrop" ,rust-gumdrop-0.8)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-i18n-embed" ,rust-i18n-embed-0.13)
                       ("rust-i18n-embed-fl" ,rust-i18n-embed-fl-0.6)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-p256" ,rust-p256-0.9)
                       ("rust-pcsc" ,rust-pcsc-2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rust-embed" ,rust-rust-embed-6)
                       ("rust-sha2" ,rust-sha2-0.9)
                       ("rust-which" ,rust-which-4)
                       ("rust-x509" ,rust-x509-0.2)
                       ("rust-x509-parser" ,rust-x509-parser-0.12)
                       ("rust-yubikey" ,rust-yubikey-0.5))
       #:cargo-development-inputs (("rust-flate2" ,rust-flate2-1)
                                   ("rust-man" ,rust-man-0.3))))
    (home-page "https://github.com/str4d/age-plugin-yubikey")
    (synopsis "YubiKey plugin for age clients")
    (description "YubiKey plugin for age clients")
    (license (list license:expat license:asl2.0))))

(define-public rust-blowfish-0.9
  (package
    (name "rust-blowfish")
    (version "0.9.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "blowfish" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1mw7bvj3bg5w8vh9xw9xawqh7ixk2xwsxkj34ph96b9b1z6y44p4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-cipher" ,rust-cipher-0.4))
       #:cargo-development-inputs (("rust-cipher" ,rust-cipher-0.4))))
    (home-page "https://github.com/RustCrypto/block-ciphers")
    (synopsis "Blowfish block cipher")
    (description "Blowfish block cipher")
    (license (list license:expat license:asl2.0))))

(define-public rust-pbkdf2-0.10.1
  (package
    (name "rust-pbkdf2")
    (version "0.10.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pbkdf2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1myz799hi58qxdxc9cch3q2sl0vs68vmgrd3j7dmc6aqbgrpj5r7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-digest" ,rust-digest-0.10)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-password-hash" ,rust-password-hash-0.3)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-sha-1" ,rust-sha-1-0.10)
                       ("rust-sha2" ,rust-sha2-0.10))
       #:cargo-development-inputs (("rust-hex-literal" ,rust-hex-literal-0.3)
                                   ("rust-hmac" ,rust-hmac-0.12)
                                   ("rust-sha-1" ,rust-sha-1-0.10)
                                   ("rust-sha2" ,rust-sha2-0.10)
                                   ("rust-streebog" ,rust-streebog-0.10))))
    (home-page
     "https://github.com/RustCrypto/password-hashes/tree/master/pbkdf2")
    (synopsis "Generic implementation of PBKDF2")
    (description "Generic implementation of PBKDF2")
    (license (list license:expat license:asl2.0))))

(define-public rust-bcrypt-pbkdf-0.8
  (package
    (name "rust-bcrypt-pbkdf")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "bcrypt-pbkdf" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0x10ficqcla5kzn214bchq71iv9113yrw3ib1f1cgfcwz8zj7vzl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-blowfish" ,rust-blowfish-0.9)
                       ("rust-pbkdf2" ,rust-pbkdf2-0.10.1)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-hex-literal" ,rust-hex-literal-0.3))))
    (home-page
     "https://github.com/RustCrypto/password-hashes/tree/master/bcrypt-pbkdf")
    (synopsis "bcrypt-pbkdf password-based key derivation function")
    (description "bcrypt-pbkdf password-based key derivation function")
    (license (list license:expat license:asl2.0))))

(define-public rust-cbc-0.1
  (package
    (name "rust-cbc")
    (version "0.1.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cbc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "19l9y9ccv1ffg6876hshd123f2f8v7zbkc4nkckqycxf8fajmd96"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cipher" ,rust-cipher-0.4))
       #:cargo-development-inputs (("rust-aes" ,rust-aes-0.8)
                                   ("rust-cipher" ,rust-cipher-0.4)
                                   ("rust-hex-literal" ,rust-hex-literal-0.3))))
    (home-page "https://github.com/RustCrypto/block-modes")
    (synopsis "Cipher Block Chaining (CBC) block cipher mode of operation")
    (description "Cipher Block Chaining (CBC) block cipher mode of operation")
    (license (list license:expat license:asl2.0))))

(define-public rust-pinentry-0.5
  (package
    (name "rust-pinentry")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pinentry" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1j4grhhn4q3f4dycnkxr0lcvlv417ysnvs2fm0mmwsmyd2ybi9dz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-secrecy" ,rust-secrecy-0.8)
                       ("rust-which" ,rust-which-4)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/str4d/pinentry-rs")
    (synopsis "API for interacting with pinentry binaries")
    (description "API for interacting with pinentry binaries")
    (license (list license:expat license:asl2.0))))

(define-public rust-rpassword-6
  (package
    (name "rust-rpassword")
    (version "6.0.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rpassword" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0mnrpxvai78mn9wqkqx8wp1gd280jjhn29ixd1dm84l6i2hrkw1b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/conradkleinespel/rpassword")
    (synopsis "Read passwords in console applications.")
    (description "Read passwords in console applications.")
    (license license:asl2.0)))

(define-public rust-salsa20-0.10
  (package
    (name "rust-salsa20")
    (version "0.10.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "salsa20" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "04w211x17xzny53f83p8f7cj7k2hi8zck282q5aajwqzydd2z8lp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cipher" ,rust-cipher-0.4))
       #:cargo-development-inputs (("rust-cipher" ,rust-cipher-0.4)
                                   ("rust-hex-literal" ,rust-hex-literal-0.3))))
    (home-page "https://github.com/RustCrypto/stream-ciphers")
    (synopsis "Salsa20 Stream Cipher")
    (description "Salsa20 Stream Cipher")
    (license (list license:expat license:asl2.0))))

(define-public rust-scrypt-0.9
  (package
    (name "rust-scrypt")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "scrypt" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0nhrr3nb44sna7gy7ny71y0rjikmwyfpfj8aq51dkw7z24wsy2ms"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-hmac" ,rust-hmac-0.12)
                       ("rust-password-hash" ,rust-password-hash-0.3)
                       ("rust-pbkdf2" ,rust-pbkdf2-0.10)
                       ("rust-salsa20" ,rust-salsa20-0.10)
                       ("rust-sha2" ,rust-sha2-0.10))
       #:cargo-development-inputs (("rust-password-hash" ,rust-password-hash-0.3))))
    (home-page
     "https://github.com/RustCrypto/password-hashes/tree/master/scrypt")
    (synopsis "Scrypt password-based key derivation function")
    (description "Scrypt password-based key derivation function")
    (license (list license:expat license:asl2.0))))

(define-public rust-wsl-0.1
  (package
    (name "rust-wsl")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wsl" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1m2hg4sfnrwm67fb1vsgycy36l2hbgmcpgllcpmbs427hsnbgnpq"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/Dentosal/wsl-rs")
    (synopsis
     "Detect if the program is running under Windows Subsystem for Linux")
    (description
     "Detect if the program is running under Windows Subsystem for Linux")
    (license license:expat)))

(define-public rust-str-stack-0.1
  (package
    (name "rust-str-stack")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "str-stack" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1sxl8xd8kiaffsryqpfwcb02lnd3djfin7gf38ag5980908vd4ch"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/Stebalien/str_stack")
    (synopsis
     "A string allocator for allocating many write-once strings.

This library is primarily useful for parsing where you need to repeatedly build
many strings, use them, and then throw them away. Instead of allocating many independent strings, this library will put them all in the same buffer.
")
    (description
     "This package provides a string allocator for allocating many write-once strings.

This library is primarily useful for parsing where you need to repeatedly build
many strings, use them, and then throw them away.  Instead of allocating many
independent strings, this library will put them all in the same buffer.")
    (license (list license:expat license:asl2.0))))

(define-public rust-testing-logger-0.1
  (package
    (name "rust-testing-logger")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "testing-logger" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "087pi7y9iisspafyzblj41qvrw95dfb6px7pavlkmls5rckvg4kd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-log" ,rust-log-0.4))))
    (home-page "https://github.com/brucechapman/rust_testing_logger")
    (synopsis "Supports writing tests to verify `log` crate calls")
    (description "Supports writing tests to verify `log` crate calls")
    (license license:bsd-3)))

(define-public rust-inferno-0.11
  (package
    (name "rust-inferno")
    (version "0.11.9")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "inferno" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "146db2vgr4wyai657c2mh93z3zq8pdjdaz00c92v7k4g0vcsacdv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-ahash" ,rust-ahash-0.7)
                       ("rust-atty" ,rust-atty-0.2)
                       ("rust-clap" ,rust-clap-3)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                       ("rust-dashmap" ,rust-dashmap-5)
                       ("rust-env-logger" ,rust-env-logger-0.9)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-num-format" ,rust-num-format-0.4)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-quick-xml" ,rust-quick-xml-0.23)
                       ("rust-rgb" ,rust-rgb-0.8)
                       ("rust-str-stack" ,rust-str-stack-0.1))
       #:cargo-development-inputs (("rust-assert-cmd" ,rust-assert-cmd-2)
                                   ("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-libflate" ,rust-libflate-1)
                                   ("rust-maplit" ,rust-maplit-1)
                                   ("rust-pretty-assertions" ,rust-pretty-assertions-1)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-testing-logger" ,rust-testing-logger-0.1))))
    (home-page "https://github.com/jonhoo/inferno.git")
    (synopsis "Rust port of the FlameGraph performance profiling tool suite")
    (description
     "Rust port of the FlameGraph performance profiling tool suite")
    (license license:cddl1.0)))

(define-public rust-prost-0.10
  (package
    (name "rust-prost")
    (version "0.10.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "prost" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gh81qwzpi04cfxiypddpad9pvcdssy31fv9zjpdm84anqfz9bbi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-prost-derive" ,rust-prost-derive-0.10))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-env-logger" ,rust-env-logger-0.8)
                                   ("rust-log" ,rust-log-0.4)
                                   ("rust-proptest" ,rust-proptest-1)
                                   ("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/tokio-rs/prost")
    (synopsis "A Protocol Buffers implementation for the Rust Language.")
    (description
     "This package provides a Protocol Buffers implementation for the Rust Language.")
    (license license:asl2.0)))

(define-public rust-prost-types-0.10
  (package
    (name "rust-prost-types")
    (version "0.10.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "prost-types" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0s0y8sc045xjynikw7n9ywm0z39fdkna3j39ivf1241n551022id"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-prost" ,rust-prost-0.10))
       #:cargo-development-inputs (("rust-proptest" ,rust-proptest-1))))
    (home-page "https://github.com/tokio-rs/prost")
    (synopsis "A Protocol Buffers implementation for the Rust Language.")
    (description
     "This package provides a Protocol Buffers implementation for the Rust Language.")
    (license license:asl2.0)))

(define-public rust-prost-build-0.10
  (package
    (name "rust-prost-build")
    (version "0.10.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "prost-build" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1aqc9cjrfwd5kh65xig0vp4cs8dhaqya7pn0kxd83mb2hwwa9rca"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cmake" ,rust-cmake-0.1)
                       ("rust-heck" ,rust-heck-0.4)
                       ("rust-itertools" ,rust-itertools-0.10)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-multimap" ,rust-multimap-0.8)
                       ("rust-petgraph" ,rust-petgraph-0.6)
                       ("rust-prost" ,rust-prost-0.10)
                       ("rust-prost-types" ,rust-prost-types-0.10)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-which" ,rust-which-4))
       #:cargo-development-inputs (("rust-env-logger" ,rust-env-logger-0.8))))
    (home-page "https://github.com/tokio-rs/prost")
    (synopsis "A Protocol Buffers implementation for the Rust Language.")
    (description
     "This package provides a Protocol Buffers implementation for the Rust Language.")
    (license license:asl2.0)))

(define-public rust-prost-derive-0.10
  (package
    (name "rust-prost-derive")
    (version "0.10.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "prost-derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1k77nir4xa06gbsdjzlygyv73razj9d11dnvxd18byspv92hyrvv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-itertools" ,rust-itertools-0.10)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/tokio-rs/prost")
    (synopsis "A Protocol Buffers implementation for the Rust Language.")
    (description
     "This package provides a Protocol Buffers implementation for the Rust Language.")
    (license license:asl2.0)))

(define-public rust-msvc-demangler-0.9
  (package
    (name "rust-msvc-demangler")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "msvc-demangler" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1j7kkmbd9yvhk7dmvd29pqcg9mcjdw5p1ia1kihh16zss1nprdmz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-1))))
    (home-page "https://github.com/mstange/msvc-demangler-rust")
    (synopsis
     "A rust library that demangles / undecorates C++ symbols mangled by MSVC")
    (description
     "This package provides a rust library that demangles / undecorates C++ symbols
mangled by MSVC")
    (license (list license:expat license:ncsa))))

(define-public rust-debugid-0.7
  (package
    (name "rust-debugid")
    (version "0.7.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "debugid" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0c370jsmnb0ahssda6f11l72ns1xpqrcmswa6y2zhknq66pqgvnn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-uuid" ,rust-uuid-0.8))
       #:cargo-development-inputs (("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://sentry.io/")
    (synopsis "Common reusable types for implementing the sentry.io protocol.")
    (description
     "Common reusable types for implementing the sentry.io protocol.")
    (license license:asl2.0)))

(define-public rust-symbolic-common-8
  (package
    (name "rust-symbolic-common")
    (version "8.8.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "symbolic-common" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0h7mcqwrhi0n6qb1h1vfz5m94dqhlqhr0spfk81mhbk4sl1gjlgm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-debugid" ,rust-debugid-0.7)
                       ("rust-memmap2" ,rust-memmap2-0.5)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-stable-deref-trait" ,rust-stable-deref-trait-1)
                       ("rust-uuid" ,rust-uuid-0.8))
       #:cargo-development-inputs (("rust-similar-asserts" ,rust-similar-asserts-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/getsentry/symbolic")
    (synopsis
     "Common types and utilities for symbolic, a library to symbolicate and process
stack traces from native applications, minidumps, minified JavaScripts or
ProGuard optimized Android apps.
")
    (description
     "Common types and utilities for symbolic, a library to symbolicate and process
stack traces from native applications, minidumps, minified JavaScripts or
ProGuard optimized Android apps.")
    (license license:expat)))

(define-public rust-symbolic-demangle-8
  (package
    (name "rust-symbolic-demangle")
    (version "8.8.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "symbolic-demangle" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0l1p8j37n7xkg8b6fgipqkcvbxl0w0kcxfwbm82l3cbf9rxwlr25"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-cpp-demangle" ,rust-cpp-demangle-0.3)
                       ("rust-msvc-demangler" ,rust-msvc-demangler-0.9)
                       ("rust-rustc-demangle" ,rust-rustc-demangle-0.1)
                       ("rust-symbolic-common" ,rust-symbolic-common-8))
       #:cargo-development-inputs (("rust-similar-asserts" ,rust-similar-asserts-1))))
    (home-page "https://github.com/getsentry/symbolic")
    (synopsis
     "A library to demangle symbols from various languages and compilers.
")
    (description
     "This package provides a library to demangle symbols from various languages and
compilers.")
    (license license:expat)))

(define-public rust-smallvec-1.7
  (package
    (name "rust-smallvec")
    (version "1.7.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "smallvec" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "02gka690j8l12gl50ifg7axqnx1m6v6d1byaq0wl3fx66p3vdjhy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-bincode" ,rust-bincode-1))))
    (home-page "https://github.com/servo/rust-smallvec")
    (synopsis
     "'Small vector' optimization: store up to a small number of items on the stack")
    (description
     "'Small vector' optimization: store up to a small number of items on the stack")
    (license (list license:expat license:asl2.0))))

(define-public rust-pprof-0.8
  (package
    (name "rust-pprof")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pprof" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0c55mmzv2mc3hq6jp8lar4vsq4p69x9d3vp0mka6kavy82944cpj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-criterion" ,rust-criterion-0.3)
                       ("rust-findshlibs" ,rust-findshlibs-0.10)
                       ("rust-inferno" ,rust-inferno-0.11)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-nix" ,rust-nix-0.23)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-prost" ,rust-prost-0.10)
                       ("rust-prost-build" ,rust-prost-build-0.10)
                       ("rust-prost-derive" ,rust-prost-derive-0.10)
                       ("rust-protobuf" ,rust-protobuf-2)
                       ("rust-protobuf-codegen-pure" ,rust-protobuf-codegen-pure-2)
                       ("rust-smallvec" ,rust-smallvec-1.7)
                       ("rust-symbolic-demangle" ,rust-symbolic-demangle-8)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/tikv/pprof-rs")
    (synopsis "An internal perf tools for rust programs.")
    (description "An internal perf tools for rust programs.")
    (license license:asl2.0)))

(define-public rust-test-case-macros-2
  (package
    (name "rust-test-case-macros")
    (version "2.2.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "test-case-macros" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "09jvbfvz48v6ya3i25gp3lbr6ym1fz7qyp3l6bcdslwkw7v7nnz4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-proc-macro-error" ,rust-proc-macro-error-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/frondeus/test-case")
    (synopsis
     "Provides #[test_case(...)] procedural macro attribute for generating parametrized test cases easily")
    (description
     "This package provides #[test_case(...)] procedural macro attribute for
generating parametrized test cases easily")
    (license license:expat)))

(define-public rust-test-case-2
  (package
    (name "rust-test-case")
    (version "2.2.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "test-case" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1h4qymhy332lzgg79w696qfxg6wdab5birn8xvfgkczzgmdczmi1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-regex" ,rust-regex-1)
                       ("rust-test-case-macros" ,rust-test-case-macros-2))
       #:cargo-development-inputs (("rust-indexmap" ,rust-indexmap-1)
                                   ("rust-insta" ,rust-insta-1)
                                   ("rust-itertools" ,rust-itertools-0.10)
                                   ("rust-linked-hash-map" ,rust-linked-hash-map-0.5)
                                   ("rust-once-cell" ,rust-once-cell-1)
                                   ("rust-regex" ,rust-regex-1)
                                   ("rust-serde-yaml" ,rust-serde-yaml-0.8))))
    (home-page "https://github.com/frondeus/test-case")
    (synopsis
     "Provides #[test_case(...)] procedural macro attribute for generating parametrized test cases easily")
    (description
     "This package provides #[test_case(...)] procedural macro attribute for
generating parametrized test cases easily")
    (license license:expat)))

(define-public rust-age-0.8
  (package
    (name "rust-age")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "age" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "18jnhbp7ysghbrq5zclqsq39nbix6glk220yngm02hnj2hawwrph"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-build-flags '("--release" "--features" "plugin")
       #:cargo-inputs (("rust-aes" ,rust-aes-0.8)
                       ("rust-age-core" ,rust-age-core-0.8)
                       ("rust-atty" ,rust-atty-0.2)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-bcrypt-pbkdf" ,rust-bcrypt-pbkdf-0.8)
                       ("rust-bech32" ,rust-bech32-0.8)
                       ("rust-cbc" ,rust-cbc-0.1)
                       ("rust-chacha20poly1305" ,rust-chacha20poly1305-0.9)
                       ("rust-cipher" ,rust-cipher-0.4)
                       ("rust-console" ,rust-console-0.15)
                       ("rust-cookie-factory" ,rust-cookie-factory-0.3)
                       ("rust-ctr" ,rust-ctr-0.9)
                       ("rust-curve25519-dalek" ,rust-curve25519-dalek-3)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-hkdf" ,rust-hkdf-0.12)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-i18n-embed" ,rust-i18n-embed-0.13)
                       ("rust-i18n-embed-fl" ,rust-i18n-embed-fl-0.6)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-pin-project" ,rust-pin-project-1)
                       ("rust-pinentry" ,rust-pinentry-0.5)
                       ("rust-rand" ,rust-rand-0.7)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rpassword" ,rust-rpassword-6)
                       ("rust-rsa" ,rust-rsa-0.5)
                       ("rust-rust-embed" ,rust-rust-embed-6)
                       ("rust-scrypt" ,rust-scrypt-0.9)
                       ("rust-sha2" ,rust-sha2-0.9)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-web-sys" ,rust-web-sys-0.3)
                       ("rust-which" ,rust-which-4)
                       ("rust-wsl" ,rust-wsl-0.1)
                       ("rust-x25519-dalek" ,rust-x25519-dalek-1)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-criterion-cycles-per-byte" ,rust-criterion-cycles-per-byte-0.1)
                                   ("rust-futures-test" ,rust-futures-test-0.3)
                                   ("rust-hex" ,rust-hex-0.4)
                                   ("rust-i18n-embed" ,rust-i18n-embed-0.13)
                                   ("rust-pprof" ,rust-pprof-0.8)
                                   ("rust-quickcheck" ,rust-quickcheck-1)
                                   ("rust-quickcheck-macros" ,rust-quickcheck-macros-1)
                                   ("rust-test-case" ,rust-test-case-2))))
    (home-page "https://github.com/str4d/rage")
    (synopsis "[BETA] A simple, secure, and modern encryption library.")
    (description "[BETA] A simple, secure, and modern encryption library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-fuse-0.3
  (package
    (name "rust-fuse")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "fuse" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0q46kr5z0d0ljallydb417dwgcd2ib5q2ak6jgpvyrh9a5q71rc0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.3)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-thread-scoped" ,rust-thread-scoped-1)
                       ("rust-time" ,rust-time-0.1))
       #:cargo-development-inputs (("rust-env-logger" ,rust-env-logger-0.3))))
    (home-page "https://github.com/zargony/rust-fuse")
    (synopsis "Rust library for filesystems in userspace (FUSE)")
    (description "Rust library for filesystems in userspace (FUSE)")
    (license license:expat)))

(define-public rust-fuse-mt-0.5
  (package
    (name "rust-fuse-mt")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "fuse-mt" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1yksh6j3j8695yliab6vkzib0b1kc9wmnsqs60rkkwjwfvxr2ijr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-fuse" ,rust-fuse-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-threadpool" ,rust-threadpool-1)
                       ("rust-time" ,rust-time-0.1))))
    (home-page "https://github.com/wfraser/fuse-mt")
    (synopsis
     "A higher-level FUSE filesystem library with multi-threading and inode->path translation.")
    (description
     "This package provides a higher-level FUSE filesystem library with
multi-threading and inode->path translation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-thread-scoped-1
  (package
    (name "rust-thread-scoped")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "thread-scoped" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "16dxl8grpii4vh20qikv2x7r871ggsf9m733xysv1lz506inmfxw"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/arcnmx/thread-scoped-rs")
    (synopsis "Unsafe and deprecated std::thread::scoped")
    (description "Unsafe and deprecated std::thread::scoped")
    (license (list license:expat license:asl2.0))))

(define-public rust-io-lifetimes-0.7
  (package
    (name "rust-io-lifetimes")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "io-lifetimes" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01ic8kxvfjkspvrfxqnrgklj5mbaj1kxg8mvhidygp85bhspz8qy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-async-std" ,rust-async-std-1)
        ("rust-fs-err" ,rust-fs-err-2)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-mio" ,rust-mio-0.8)
        ("rust-os-pipe" ,rust-os-pipe-1)
        ("rust-socket2" ,rust-socket2-0.4)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-windows-sys" ,rust-windows-sys-0.36))))
    (home-page "https://github.com/sunfishcode/io-lifetimes")
    (synopsis "A low-level I/O ownership and borrowing library")
    (description
     "This package provides a low-level I/O ownership and borrowing library")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-linux-raw-sys-0.0.46
  (package
    (name "rust-linux-raw-sys")
    (version "0.0.46")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "linux-raw-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kc528mp2fp8m96csm6rmwg0ac7zbgf36k19ml4a4c9j6xn4blnl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
        ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))
       #:cargo-development-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-static-assertions" ,rust-static-assertions-1))))
    (home-page "https://github.com/sunfishcode/linux-raw-sys")
    (synopsis "Generated bindings for Linux's userspace API")
    (description "Generated bindings for Linux's userspace API")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-rustix-0.35
  (package
    (name "rust-rustix")
    (version "0.35.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustix" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1vvw8565cv1dnwi74iw5884a2hif82mk2m5hn4ri9vvdcsjgvcpv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cc" ,rust-cc-1)
        ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
        ("rust-errno" ,rust-errno-0.2)
        ("rust-io-lifetimes" ,rust-io-lifetimes-0.7)
        ("rust-itoa" ,rust-itoa-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-linux-raw-sys" ,rust-linux-raw-sys-0.0.46)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
        ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
        ("rust-windows-sys" ,rust-windows-sys-0.36))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3)
        ("rust-ctor" ,rust-ctor-0.1)
        ("rust-errno" ,rust-errno-0.2)
        ("rust-io-lifetimes" ,rust-io-lifetimes-0.7)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-memoffset" ,rust-memoffset-0.6)
        ("rust-serial-test" ,rust-serial-test-0.6)
        ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/bytecodealliance/rustix")
    (synopsis "Safe Rust bindings to POSIX/Unix/Linux/Winsock2-like syscalls")
    (description
     "Safe Rust bindings to POSIX/Unix/Linux/Winsock2-like syscalls")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-terminal-size-0.2
  (package
    (name "rust-terminal-size")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "terminal-size" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "18f57ag083ckf460wyhp34jdh193rhxrh2ja9qbgdpkrrxhchh44"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-rustix" ,rust-rustix-0.35)
                       ("rust-windows-sys" ,rust-windows-sys-0.36))))
    (home-page "https://github.com/eminence/terminal-size")
    (synopsis "Gets the size of your Linux or Windows terminal")
    (description "Gets the size of your Linux or Windows terminal")
    (license (list license:expat license:asl2.0))))

(define-public rust-unic-emoji-char-0.9
  (package
    (name "rust-unic-emoji-char")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "unic-emoji-char" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ka9fr7s6lv0z43r9xphg9injn35pfxf9g9q18ki0wl9d0g241qb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-unic-char-property" ,rust-unic-char-property-0.9)
                       ("rust-unic-char-range" ,rust-unic-char-range-0.9)
                       ("rust-unic-ucd-version" ,rust-unic-ucd-version-0.9))))
    (home-page "https://github.com/open-i18n/rust-unic/")
    (synopsis "UNIC Unicode Emoji Emoji Character Properties")
    (description "UNIC Unicode Emoji Emoji Character Properties")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-3
  (package
    (name "rust-clap")
    (version "3.2.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "clap" version))
              (file-name (string-append name "-" version ".tar.gz"))
             (sha256
               (base32
                "1jf5yh8321zqhxzcz5fdhjjqrxxr5682z2lsj3gpsbzmlmpmcdm8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-atty" ,rust-atty-0.2)
                       ("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-clap-derive" ,rust-clap-derive-3)
                       ("rust-clap-lex" ,rust-clap-lex-0.2)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-strsim" ,rust-strsim-0.10)
                       ("rust-termcolor" ,rust-termcolor-1)
                       ("rust-terminal-size" ,rust-terminal-size-0.1)
                       ("rust-textwrap" ,rust-textwrap-0.15)
                       ("rust-unicase" ,rust-unicase-2)
                       ("rust-yaml-rust" ,rust-yaml-rust-0.4))
       #:cargo-development-inputs (("rust-humantime" ,rust-humantime-2)
                                   ("rust-regex" ,rust-regex-1)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-shlex" ,rust-shlex-1)
                                   ("rust-snapbox" ,rust-snapbox-0.2)
                                   ("rust-trybuild" ,rust-trybuild-1)
                                   ("rust-trycmd" ,rust-trycmd-0.13))))
    (home-page "https://github.com/clap-rs/clap")
    (synopsis
     "A simple to use, efficient, and full-featured Command Line Argument Parser")
    (description
     "This package provides a simple to use, efficient, and full-featured Command Line
Argument Parser")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-lex-0.2
  (package
    (name "rust-clap-lex")
    (version "0.2.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "clap-lex" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ib1a9v55ybnaws11l63az0jgz5xiy24jkdgsmyl7grcm3sz4l18"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-os-str-bytes" ,rust-os-str-bytes-6))))
    (home-page "https://github.com/clap-rs/clap/tree/master/clap_lex")
    (synopsis "Minimal, flexible command line parser")
    (description "Minimal, flexible command line parser")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-complete-3
  (package
    (name "rust-clap-complete")
    (version "3.2.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "clap-complete" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1n3whjkznszrxif1hzvql7hav7agq85j456fmwjwwi9cjq52wyiz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-clap" ,rust-clap-3)
                       ("rust-clap-lex" ,rust-clap-lex-0.2)
                       ("rust-is-executable" ,rust-is-executable-1)
                       ("rust-os-str-bytes" ,rust-os-str-bytes-6)
                       ("rust-pathdiff" ,rust-pathdiff-0.2)
                       ("rust-shlex" ,rust-shlex-1)
                       ("rust-unicode-xid" ,rust-unicode-xid-0.2))
       #:cargo-development-inputs (("rust-clap" ,rust-clap-3)
                                   ("rust-pretty-assertions" ,rust-pretty-assertions-1)
                                   ("rust-snapbox" ,rust-snapbox-0.2)
                                   ("rust-trycmd" ,rust-trycmd-0.13))))
    (home-page "https://github.com/clap-rs/clap/tree/master/clap_complete")
    (synopsis "Generate shell completion scripts for your clap::Command")
    (description "Generate shell completion scripts for your clap::Command")
    (license (list license:expat license:asl2.0))))

(define-public rage
  (package
    (name "rage")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rage" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "164csgcn45ji325c143ywahv60iaarwvr81s4l008d0cr5dqblva"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-age" ,rust-age-0.8)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-console" ,rust-console-0.15)
        ("rust-env-logger" ,rust-env-logger-0.9)
        ("rust-fuse-mt" ,rust-fuse-mt-0.5)
        ("rust-gumdrop" ,rust-gumdrop-0.8)
        ("rust-i18n-embed" ,rust-i18n-embed-0.13)
        ("rust-i18n-embed-fl" ,rust-i18n-embed-fl-0.6)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-pinentry" ,rust-pinentry-0.5)
        ("rust-rust-embed" ,rust-rust-embed-6)
        ("rust-tar" ,rust-tar-0.4)
        ("rust-time" ,rust-time-0.1)
        ("rust-zip" ,rust-zip-0.5))
       #:cargo-development-inputs
       (("rust-clap" ,rust-clap-3)
        ("rust-clap-complete" ,rust-clap-complete-3)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-man" ,rust-man-0.3))))
    (home-page "https://github.com/str4d/rage")
    (synopsis "[BETA] A simple, secure, and modern encryption tool.")
    (description "[BETA] A simple, secure, and modern encryption tool.")
    (license (list license:expat license:asl2.0))))
