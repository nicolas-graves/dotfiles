(define-module (packages swayr)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (nongnu packages mozilla)
  #:use-module (gnu packages)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages compression))

(define-public rust-thiserror-impl-1
  (package
    (name "rust-thiserror-impl")
    (version "1.0.37")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "thiserror-impl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fydmpksd14x1mkc24zas01qjssz8q43sbn2ywl6n527dda1fbcq"))))
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
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "thiserror" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gky83x4i87gd87w3fknnp920wvk9yycp7dgkf5h3jg364vb7phh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-thiserror-impl" ,rust-thiserror-impl-1))))
    (home-page "https://github.com/dtolnay/thiserror")
    (synopsis "derive(Error)")
    (description "derive(Error)")
    (license (list license:expat license:asl2.0))))

(define-public rust-swayipc-types-1
  (package
   (name "rust-swayipc-types")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "swayipc-types" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13lj6jyyxg41r9g0b07y8yd7ygy5gih61w5v48bpksvfdzhwwn55"))))
   (build-system cargo-build-system)
   (arguments
    `(#:skip-build?
      #t
      #:cargo-inputs
      (("rust-serde" ,rust-serde-1)
       ("rust-serde-json" ,rust-serde-json-1)
       ("rust-thiserror" ,rust-thiserror-1))))
   (home-page "https://github.com/jaycefayne/swayipc-rs")
   (synopsis "A library containing Type defintions from sway's IPC interface")
   (description
    "This package provides a library containing Type defintions from sway's IPC
interface")
   (license license:expat)))

(define-public rust-swayipc-3
  (package
    (name "rust-swayipc")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "swayipc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16pf4r6svf99p73b8dhdannkvhfvmbjb4rx7gifxh8xj53rwy7db"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build?
       #t
       #:cargo-inputs
       (("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-swayipc-types" ,rust-swayipc-types-1))))
    (home-page "https://github.com/jaycefayne/swayipc-rs")
    (synopsis "A library for controlling sway through its IPC interface")
    (description
     "This package provides a library for controlling sway through its IPC interface")
    (license license:expat)))

(define-public rust-rt-format-0.3
  (package
    (name "rust-rt-format")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rt-format" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qjjwh9ny95xck1kp99gi6hfm9glrx54jx8npnj6yccxc7p7q225"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-regex" ,rust-regex-1))))
    (home-page "https://github.com/vstojkovic/rt-format")
    (synopsis "Fully-runtime equivalent of the format! macro")
    (description "Fully-runtime equivalent of the format! macro")
    (license license:asl2.0)))

(define-public rust-clap-derive-3
  (package
   (name "rust-clap-derive")
   (version "3.1.18")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "clap_derive" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0g53w6qkqcc122bqh51jzfg51147il643idvq1czxkr2x5306ci5"))))
   (build-system cargo-build-system)
   (arguments
    `(#:skip-build?
      #t
      #:cargo-inputs
      (("rust-heck" ,rust-heck-0.4)
       ("rust-proc-macro-error" ,rust-proc-macro-error-1)
       ("rust-proc-macro2" ,rust-proc-macro2-1)
       ("rust-quote" ,rust-quote-1)
       ("rust-syn" ,rust-syn-1))))
   (home-page "https://github.com/clap-rs/clap/tree/master/clap_derive")
   (synopsis
    "Parse command line argument by defining a struct, derive crate.")
   (description
    "Parse command line argument by defining a struct, derive crate.")
   (license (list license:expat license:asl2.0))))

(define-public rust-clap-3
  (package
   (name "rust-clap")
   (version "3.1.18")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "clap" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "02s4hk9hrmm2s1j7dkbwpyd75mfzx3p8ks2chmp4ccybv95xznyj"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs
      (("rust-atty" ,rust-atty-0.2)
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
      #:cargo-development-inputs
      (("rust-criterion" ,rust-criterion-0.3)
       ("rust-humantime" ,rust-humantime-2)
       ("rust-lazy-static" ,rust-lazy-static-1)
       ("rust-regex" ,rust-regex-1)
       ("rust-rustversion" ,rust-rustversion-1)
       ("rust-shlex" ,rust-shlex-1)
       ("rust-snapbox" ,rust-snapbox-0.4)
       ("rust-trybuild" ,rust-trybuild-1)
       ("rust-trycmd" ,rust-trycmd-0.13))))
   (home-page "https://github.com/clap-rs/clap")
   (synopsis
    "A simple to use, efficient, and full-featured Command Line Argument Parser")
   (description
    "This package provides a simple to use, efficient, and full-featured Command Line
Argument Parser")
   (license (list license:expat license:asl2.0))))

(define-public swayr
  (package
   (name "swayr")
   (version "0.18.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "swayr" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1m443lwbs3lm20kkviw60db56w9i59dm393z1sn6llpfi2xihh3h"))))
   (build-system cargo-build-system)
   (arguments
    `(#:tests?
      #f
      #:cargo-inputs
      (("rust-clap" ,rust-clap-3)
       ("rust-directories" ,rust-directories-4)
       ("rust-env-logger" ,rust-env-logger-0.9)
       ("rust-log" ,rust-log-0.4)
       ("rust-once-cell" ,rust-once-cell-1)
       ("rust-rand" ,rust-rand-0.8)
       ("rust-regex" ,rust-regex-1)
       ("rust-rt-format" ,rust-rt-format-0.3)
       ("rust-serde" ,rust-serde-1)
       ("rust-serde-json" ,rust-serde-json-1)
       ("rust-swayipc" ,rust-swayipc-3)
       ("rust-toml" ,rust-toml-0.5))))
   (home-page "https://sr.ht/~tsdh/swayr/")
   (synopsis "A LRU window-switcher (and more) for the sway window manager")
   (description
    "This package provides a LRU window-switcher (and more) for the sway window
manager")
   (license license:gpl3+)))
