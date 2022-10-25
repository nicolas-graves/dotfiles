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

(define-public rust-serde-json-1
  (package
    (name "rust-serde-json")
    (version "1.0.79")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde-json" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
          (base32 "158xd1swdvw6y59bx4avb8vdpj727n54r77xw5f7c15kqfjrz3cf"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t
        #:cargo-inputs
        (("rust-indexmap" ,rust-indexmap-1)
         ("rust-itoa" ,rust-itoa-1)
         ("rust-ryu" ,rust-ryu-1)
         ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/serde-rs/json")
    (synopsis "JSON serialization file format")
    (description
     "This package provides a JSON serialization file format.")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-derive-1
  (package
    (name "rust-serde-derive")
    (version "1.0.136")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yb28smlymba4qbj2bn4c4myvblypqvkxv9q33s0dlzwa9qpwn88"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
                     #:cargo-inputs
                     (("rust-proc-macro2" ,rust-proc-macro2-1)
                      ("rust-quote" ,rust-quote-1)
                      ("rust-syn" ,rust-syn-1))))
    (home-page "https://serde.rs")
    (synopsis
     "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
     "Macros 1.1 implementation of #[derive(Serialize, Deserialize)].")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-1
  (package
    (name "rust-serde")
    (version "1.0.136")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12a791cbdd3gi08536i4frrqsps0ak8gvhpijvgj9rg1055y4cff"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
                     #:cargo-inputs
                     (("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://serde.rs")
    (synopsis "Generic serialization/deserialization framework")
    (description
     "This package provides a generic serialization/deserialization framework.")
    (license (list license:expat license:asl2.0))))

(define-public rust-findshlibs-0.10
  (package
    (name "rust-findshlibs")
    (version "0.10.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "findshlibs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r3zy2r12rxzwqgz53830bk38r6b7rl8kq2br9n81q7ps2ffbfa0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc-1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'unix-cargo-toml
           (lambda _
             (setenv "CARGO_BUILD_RUSTFLAGS"
                     (string-append "--cfg 'target_os=\"linux\"'"))
             (delete-file "Cargo.toml")
             (substitute* "Cargo.toml.orig"
               (("^winapi.*") ""))
             (rename-file "Cargo.toml.orig" "Cargo.toml")
             #t)))))
    (home-page "https://github.com/gimli-rs/findshlibs")
    (synopsis "Find the set of shared libraries loaded in the current process")
    (description
     "Find the set of shared libraries loaded in the current process with a
cross platform API.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-mockall-double-0.2
  (package
    (name "rust-mockall-double")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mockall_double" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mmch8dnwrqz6v22hn128m7iaphlg7gw3s5xsa2cqvj5jxdw3zvx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/asomers/mockall")
    (synopsis "Test double adapter for Mockall")
    (description "Test double adapter for Mockall")
    (license (list license:expat license:asl2.0))))

(define-public rust-mockall-derive-0.11
  (package
    (name "rust-mockall-derive")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mockall_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sxavgqr9m6xh68fibfrh7jb4vfw9j86xqi14bvs7pm012121vvr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))
       #:cargo-development-inputs
       (("rust-pretty-assertions" ,rust-pretty-assertions-0.7))))
    (home-page "https://github.com/asomers/mockall")
    (synopsis "Procedural macros for Mockall")
    (description "This package provides procedural macros for Mockall. Mock
objects are a powerful technique for unit testing software. A mock object is
an object with the same interface as a real object, but whose responses are
all manually controlled by test code. Mockall incorporates the best elements
of previous mock Rust designs, resulting in it having a rich feature set with
a terse and ergonomic interface.")
    (license (list license:expat license:asl2.0))))

(define-public rust-downcast-0.11
  (package
    (name "rust-downcast")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "downcast" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1wa78ahlc57wmqyq2ncr80l7plrkgz57xsg7kfzgpcnqac8gld8l"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/fkoep/downcast-rs")
    (synopsis
      "Trait for downcasting trait objects back to their original types")
    (description
      "This package provides a trait for downcasting trait objects back to
their original types in Rust.")
    (license license:expat)))

(define-public rust-mockall-0.11
  (package
    (name "rust-mockall")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mockall" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1f6nscx90jkj4vza3ck8ldn2cs6savdm0qxibmr75ybjk9ip0k9x"))))
    (build-system cargo-build-system)
    (arguments
     `( ;; #:skip-build? #t
       #:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-downcast" ,rust-downcast-0.11)
        ("rust-fragile" ,rust-fragile-1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-mockall-derive" ,rust-mockall-derive-0.11)
        ("rust-predicates" ,rust-predicates-2)
        ("rust-predicates-tree" ,rust-predicates-tree-1))
       #:cargo-development-inputs
       (("rust-async-trait" ,rust-async-trait-0.1)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-mockall-double" ,rust-mockall-double-0.2))
       #:tests? #f))
    (home-page "https://github.com/asomers/mockall")
    (synopsis "A powerful mock object library for Rust")
    (description "This package provides procedural macros for Mockall. Mock
objects are a powerful technique for unit testing software. A mock object is
an object with the same interface as a real object, but whose responses are
all manually controlled by test code. Mockall incorporates the best elements
of previous mock Rust designs, resulting in it having a rich feature set with
a terse and ergonomic interface.")
    (license (list license:expat license:asl2.0))))

(define-public rust-which-4
  (package
    (name "rust-which")
    (version "4.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "which" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1ln1wmhb6k3al9zhbw8rzidr1imni55ajr3840hg474jgr47wnia"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-either" ,rust-either-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-regex" ,rust-regex-1))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3)
        ("rust-tempdir" ,rust-tempdir-0.3))
       #:tests? #f))
    (home-page "https://github.com/harryfei/which-rs.git")
    (synopsis
      "Locate installed executable")
    (description
      "This package provides a Rust equivalent of Unix command \"which\".  It
locates installed executable in cross platforms.")
    (license license:expat)))

(define-public rust-minimal-lexical-0.2
  (package
    (name "rust-minimal-lexical")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "minimal-lexical" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "16ppc5g84aijpri4jzv14rvcnslvlpphbszc7zzp6vfkddf4qdb8"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/Alexhuszagh/minimal-lexical")
    (synopsis "Fast float parsing conversion routines for Rust")
    (description "This package provides a fast float parsing conversion
routines.")
    (license (list license:expat license:asl2.0))))

(define-public rust-nom-7
  (package
    (name "rust-nom")
    (version "7.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "nom" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0djc3lq5xihnwhrvkc4bj0fd58sjf632yh6hfiw545x355d3x458"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-memchr" ,rust-memchr-2)
        ("rust-minimal-lexical" ,rust-minimal-lexical-0.2))
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-proptest" ,rust-proptest-1))
       #:tests? #f))
    (home-page "https://github.com/Geal/nom")
    (synopsis "A byte-oriented, zero-copy, parser combinators library")
    (description
     "This package provides a parser combinators library written in
Rust. Num's goal is to provide tools to build safe parsers without
compromising the speed or memory consumption. To that end, it uses extensively
Rust's strong typing and memory safety to produce fast and correct parsers,
and provides functions, macros and traits to abstract most of the error prone
plumbing.")
    (license license:expat)))

(define-public rust-semver-parser-0.10
  (package
    (name "rust-semver-parser")
    (version "0.10.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "semver-parser" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1xqijhqhx3bn77xnl1mlcp032hz8nv7n2fbdacbdzq7rnzsvxc00"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-pest" ,rust-pest-2))
        #:cargo-development-inputs
        (("rust-pest-generator" ,rust-pest-generator-2)
         ("rust-proc-macro2" ,rust-proc-macro2-1))
        #:tests? #f))
    (home-page "https://github.com/steveklabnik/semver-parser")
    (synopsis "Parsing of the semver spec.")
    (description "Parsing of the semver spec.")
    (license (list license:expat license:asl2.0))))

(define-public rust-version-sync-0.9
  (package
    (name "rust-version-sync")
    (version "0.9.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "version-sync" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1w0v20p6k13yhfmgmcwhgy3371znyqcn83lhrf47swq7xhf81l4r"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-pulldown-cmark" ,rust-pulldown-cmark-0.8)
         ("rust-regex" ,rust-regex-1)
         ("rust-semver" ,rust-semver-1)
         ("rust-syn" ,rust-syn-1)
         ("rust-toml" ,rust-toml-0.5)
         ("rust-url" ,rust-url-2))
        #:cargo-development-inputs
        (("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/mgeisler/version-sync")
    (synopsis
      "Crate for ensuring that version numbers in README files and other files are kept in sync with the crate version.")
    (description
      "Crate for ensuring that version numbers in README files and other files are kept
in sync with the crate version.")
    (license license:expat)))

(define-public rust-versions-4
  (package
    (name "rust-versions")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "versions" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1m04qvig0av1fz36082kyaln5rlndzrj70g0az6f3pck2wanq9ym"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-itertools" ,rust-itertools-0.10)
        ("rust-nom" ,rust-nom-7)
        ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs
       (("rust-version-sync" ,rust-version-sync-0.9)
        ("rust-semver" ,rust-semver-1)
        ("rust-semver-parser" ,rust-semver-parser-0.10))))
    (home-page "https://github.com/fosskers/rs-versions")
    (synopsis "A library for parsing and comparing software version numbers.")
    (description
     "This package provides a library for parsing and comparing software version
numbers.")
    (license license:expat)))

(define-public rust-urlencoding-2
  (package
    (name "rust-urlencoding")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "urlencoding" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "08cq5w84imxrpyifhmx719026dzjih29gdq0ncsb1fcs08qhkfb8"))))
    (build-system cargo-build-system)
    (home-page "https://lib.rs/urlencoding")
    (synopsis "A Rust library for doing URL percentage encoding.")
    (description
      "This package provides a Rust library for doing URL percentage encoding.")
    (license license:expat)))

(define-public rust-unicode-segmentation-1
  (package
    (name "rust-unicode-segmentation")
    (version "1.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-segmentation" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "16gxxda9aya0arcqs9aa9lb31b3i54i34dmyqi6j5xkpszsj123y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-quickcheck" ,rust-quickcheck-0.7)
        ("rust-criterion" ,rust-criterion-0.3))))
    (home-page "https://github.com/unicode-rs/unicode-segmentation")
    (synopsis
      "This crate provides Grapheme Cluster, Word and Sentence boundaries
according to Unicode Standard Annex #29 rules.")
    (description
      "This crate provides Grapheme Cluster, Word and Sentence boundaries according to
Unicode Standard Annex #29 rules.")
    (license (list license:expat license:asl2.0))))

(define-public rust-compact-str-0.3
  (package
    (name "rust-compact-str")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "compact_str" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1lhcwcymljzxp5pkyg224byhsk1b9cwdn8p3iqxmnn3r3zw8a5sc"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bytes" ,rust-bytes-1)
         ("rust-serde" ,rust-serde-1))
        #:cargo-development-inputs
        (("rust-proptest" ,rust-proptest-1))))
    (home-page "https://github.com/ParkMyCar/compact_str")
    (synopsis
      "A memory efficient immutable string type that transparently stores strings on the stack, when possible")
    (description
      "This package provides a memory efficient immutable string type that
transparently stores strings on the stack, when possible")
    (license license:expat)))

(define-public rust-kstring-1
  (package
    (name "rust-kstring")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "kstring" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09j5xb3rnjd3kmc2v667wzsc4mz4c1hl1vkzszbj30fyxb60qccb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3)
        ("rust-smol-str" ,rust-smol-str-0.1)
        ("rust-compact-str" ,rust-compact-str-0.3)
        ("rust-smartstring" ,rust-smartstring-0.2))))
    (home-page "https://github.com/cobalt-org/kstring")
    (synopsis "Key String: optimized for map keys")
    (description "Key String: optimized for map keys")
    (license (list license:expat license:asl2.0))))

(define-public rust-combine-4
  (package
    (name "rust-combine")
    (version "4.6.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "combine" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0qihymj493vvs054gzpcmp4lzb098zrj2p9miv19yzvrrjm2gdsh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes-0.5)
        ("rust-bytes" ,rust-bytes-1)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-io" ,rust-futures-io-0.3)
        ("rust-memchr" ,rust-memchr-2)
        ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
        ("rust-regex" ,rust-regex-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio" ,rust-tokio-0.3)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-util" ,rust-tokio-util-0.6))
       #:cargo-development-inputs
       (("rust-async-std" ,rust-async-std-1)
        ("rust-criterion" ,rust-criterion-0.3)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-partial-io" ,rust-partial-io-0.3)
        ("rust-quickcheck" ,rust-quickcheck-0.6)
        ("rust-quick-error" ,rust-quick-error-1))))
    (home-page "https://github.com/Marwes/combine")
    (synopsis
      "Fast parser combinators on arbitrary streams with zero-copy support.")
    (description
      "Fast parser combinators on arbitrary streams with zero-copy support.")
    (license license:expat)))

(define-public rust-include-dir-impl-0.6
  (package
    (name "rust-include-dir-impl")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "include_dir_impl" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1pw2nv1cy5483jjic5v8i9n3rgbb743wf122rrxsnjyshl68j30a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-proc-macro-hack" ,rust-proc-macro-hack-0.5)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/Michael-F-Bryan/include_dir")
    (synopsis "Implementation crate for include_dir")
    (description "Implementation crate for include_dir")
    (license license:expat)))

(define-public rust-include-dir-0.6
  (package
    (name "rust-include-dir")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "include_dir" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1arcwvyrpwm3kjxy0kz2hylc1l3hw089y0qgklgdd1v1gqa6xd94"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-glob" ,rust-glob-0.3)
        ("rust-include-dir-impl" ,rust-include-dir-impl-0.6)
        ("rust-proc-macro-hack" ,rust-proc-macro-hack-0.5))
       #:cargo-development-inputs
       (("rust-tempdir" ,rust-tempdir-0.3))
       #:tests? #f))
    (home-page "https://github.com/Michael-F-Bryan/include_dir")
    (synopsis "Embed the contents of a directory in your binary")
    (description "Embed the contents of a directory in your binary")
    (license license:expat)))

(define-public rust-toml-test-data-1
  (package
    (name "rust-toml-test-data")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "toml-test-data" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1p2762k71hssn07vsw0h114agp05hfyrymdqdvzsp1cfhcn69swr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
        (("rust-include-dir" ,rust-include-dir-0.6))))
    (home-page "https://github.com/epage/toml-test-rs")
    (synopsis "TOML test cases")
    (description "TOML test cases")
    (license (list license:expat license:asl2.0))))

(define-public rust-toml-test-0.3
  (package
    (name "rust-toml-test")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "toml-test" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1n0q8s27568xvkczyvhlannbf79k4rl5gmrl5wvazzlf8ii4ab7d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-chrono" ,rust-chrono-0.4)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1))
       #:cargo-development-inputs
       (("rust-toml-test-data" ,rust-toml-test-data-1))))
    (home-page "https://github.com/epage/toml-test-rs")
    (synopsis "Verify Rust TOML parsers")
    (description "Verify Rust TOML parsers")
    (license (list license:expat license:asl2.0))))

(define-public rust-libtest-mimic-0.3
  (package
    (name "rust-libtest-mimic")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libtest-mimic" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1bp2jllwpciljr14g6s9bk4835g46kszgrjwi66vxxsk3ynbi9q8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-crossbeam-channel" ,rust-crossbeam-channel-0.4)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-structopt" ,rust-structopt-0.3)
        ("rust-termcolor" ,rust-termcolor-1))))
    (home-page "https://github.com/LukasKalbertodt/libtest-mimic")
    (synopsis
      "Write your own test harness that looks and behaves like the built-in test harness used by `rustc --test`
")
    (description
      "Write your own test harness that looks and behaves like the built-in test
harness used by `rustc --test`")
    (license (list license:expat license:asl2.0))))

(define-public rust-toml-test-harness-0.3
  (package
    (name "rust-toml-test-harness")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "toml-test-harness" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "07w0sxlsrhf1ym8mlisdkahj5r38c6391vz0rfdxs30hxhbc2va1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-ignore" ,rust-ignore-0.4)
         ("rust-libtest-mimic" ,rust-libtest-mimic-0.3)
         ("rust-toml-test" ,rust-toml-test-0.3)
         ("rust-toml-test-data" ,rust-toml-test-data-1))))
    (home-page "https://github.com/epage/toml-test-rs")
    (synopsis "Cargo test harness for verifying TOML parsers")
    (description "Cargo test harness for verifying TOML parsers")
    (license (list license:expat license:asl2.0))))

(define-public rust-fs-snapshot-0.1
  (package
    (name "rust-fs-snapshot")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fs_snapshot" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "19hlpn7rbmgbvd9pgnz94m83wcj714m1cd57klzlhdhjw8z6qmpg"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-concolor" ,rust-concolor-0.0.8)
         ("rust-difflib" ,rust-difflib-0.4)
         ("rust-ignore" ,rust-ignore-0.4)
         ("rust-libtest-mimic" ,rust-libtest-mimic-0.3)
         ("rust-normalize-line-endings" ,rust-normalize-line-endings-0.3)
         ("rust-yansi" ,rust-yansi-0.5))))
    (home-page "https://github.com/assert-rs/cargo-api")
    (synopsis "Simple input/output file snapshotting")
    (description "Simple input/output file snapshotting")
    (license (list license:expat license:asl2.0))))

(define-public rust-pretty-assertions-1
  (package
    (name "rust-pretty-assertions")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pretty-assertions" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "12r2zd27kr3wg2i1i0a3mp0gwnr73lk7q8lwpw2cgf8rag5kih2p"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-ansi-term" ,rust-ansi-term-0.12)
         ("rust-ctor" ,rust-ctor-0.1)
         ("rust-diff" ,rust-diff-0.1)
         ("rust-output-vt100" ,rust-output-vt100-0.1))))
    (home-page "https://github.com/colin-kiegel/rust-pretty-assertions")
    (synopsis
      "Overwrite `assert_eq!` and `assert_ne!` with drop-in replacements, adding colorful diffs.")
    (description
      "Overwrite `assert_eq!` and `assert_ne!` with drop-in replacements, adding
colorful diffs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-toml-edit-0.13
  (package
    (name "rust-toml-edit")
    (version "0.13.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "toml_edit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nzmc02sfkkxsg59gjqsg0irwn5mj5lp2cz0gjj0ld2jngarwkkl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-combine" ,rust-combine-4)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-itertools" ,rust-itertools-0.10)
        ("rust-kstring" ,rust-kstring-1)
        ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs
       (("rust-serde-json" ,rust-serde-json-1)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1)
        ("rust-toml-test-harness" ,rust-toml-test-harness-0.3)
        ("rust-fs-snapshot" ,rust-fs-snapshot-0.1)
        ("rust-criterion" ,rust-criterion-0.3)
        ("rust-toml" ,rust-toml-0.5))
       #:tests? #f))
    (home-page "https://github.com/ordian/toml_edit")
    (synopsis "Yet another format-preserving TOML parser.")
    (description "Yet another format-preserving TOML parser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sys-info-0.9
  (package
    (name "rust-sys-info")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sys-info" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0b759814ng0cj5a1iiqqjgrzfg9vqlpkbp6z3l76mycbp850sfhb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc-1) ("rust-libc" ,rust-libc-0.2))
       #:tests? #f))
    (home-page "https://github.com/FillZpp/sys-info-rs")
    (synopsis "Get system information in Rust.")
    (description
      "Get system information in Rust.")
    (license license:expat)))

(define-public rust-starship-battery-0.7
  (package
    (name "rust-starship-battery")
    (version "0.7.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "starship-battery" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0p819qlmlh2qg3iw3w7qf12q29icar74zgk9mr3l9bq4s251jdik"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-lazycell" ,rust-lazycell-1)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-uom" ,rust-uom-0.30))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3)
        ("rust-approx" ,rust-approx-0.3))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'linux-cargo-toml
           (lambda _
             (setenv "CARGO_BUILD_RUSTFLAGS"
                     (string-append "--cfg 'target_os=\"linux\"'"))
             (delete-file "Cargo.toml")
             (substitute* "Cargo.toml.orig"
               (("^nix.*") "")
               (("^mach.*") "")
               (("^core-foundation.*") "")
               (("^winapi.*") "")
               (("^libc.*") ""))
             (rename-file "Cargo.toml.orig" "Cargo.toml")
             #t)))
       #:tests? #f))
    (home-page "https://github.com/starship/rust-battery")
    (synopsis "Cross-platform information about the notebook batteries")
    (description "Cross-platform information about the notebook batteries")
    (license license:isc)))

(define-public rust-shell-words-1
  (package
    (name "rust-shell-words")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "shell-words" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1plgwx8r0h5ismbbp6cp03740wmzgzhip85k5hxqrrkaddkql614"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/tmiasko/shell-words")
    (synopsis "Process command line according to parsing rules of UNIX shell")
    (description
      "Process command line according to parsing rules of UNIX shell")
    (license (list license:expat license:asl2.0))))

(define-public rust-document-features-0.2
  (package
    (name "rust-document-features")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "document-features" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "121wr2bd8a4s5i5yrxjz8c5amw2l69xmqqma86x6y4xmcgyhj75h"))))
    (build-system cargo-build-system)
    ;; (arguments `(#:skip-build? #t))
    (home-page "https://slint-ui.com")
    (synopsis
      "Extract documentation for the feature flags from comments in Cargo.toml")
    (description
      "Extract documentation for the feature flags from comments in Cargo.toml")
    (license (list license:expat license:asl2.0))))

(define-public rust-const-format-proc-macros-0.2
  (package
    (name "rust-const-format-proc-macros")
    (version "0.2.22")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "const_format_proc_macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bdqnac7qfnn2gayxazvb148nczdxn37djyyly6s8y18jxfns6gg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1)
        ("rust-unicode-xid" ,rust-unicode-xid-0.2))
       #:cargo-development-inputs
       (("rust-fastrand" ,rust-fastrand-1))))
    (home-page "https://github.com/rodrimati1992/const_format_crates/")
    (synopsis "Implementation detail of the `const_format` crate")
    (description "Implementation detail of the `const_format` crate")
    (license license:zlib)))

(define-public rust-const-format-0.2
  (package
    (name "rust-const-format")
    (version "0.2.22")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "const_format" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "055n171jydahwcnn1dqwk5rkpika5cc81qy3h2v0gi0fkga6rg12"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-const-format-proc-macros" ,rust-const-format-proc-macros-0.2))
        #:cargo-development-inputs
        (("rust-arrayvec" ,rust-arrayvec-0.5)
         ("rust-fastrand" ,rust-fastrand-1))
        #:cargo-test-flags
        '("--release" "--features" "testing")))
    (home-page "https://github.com/rodrimati1992/const_format_crates/")
    (synopsis "Compile-time string formatting")
    (description "Compile-time string formatting")
    (license license:zlib)))

(define-public rust-shadow-rs-0.9
  (package
    (name "rust-shadow-rs")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "shadow-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02gpjyf79irrxa5v22sgylwvi6az4g19y56j00zsrzbbv74wmaiq"))))
    (build-system cargo-build-system)
    (arguments
     `( ;; #:skip-build? #t
       #:cargo-inputs
       (("rust-chrono" ,rust-chrono-0.4)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-git2" ,rust-git2-0.13)
        ("rust-is-debug" ,rust-is-debug-1))
       #:cargo-test-flags
       '("--release" "--lib" "zlib")))
    (inputs (list zlib))
    (home-page "https://github.com/baoyachi/shadow-rs")
    (synopsis "A build-time information stored in your rust project")
    (description
     "This package provides a build-time information stored in your rust project")
    (license (list license:expat license:asl2.0))))

(define-public rust-semver-1
  (package
    (name "rust-semver")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "semver" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "13gkqi8szcqgn3k6agndw9kfggfa41pm6ir02y3l5lpd0cg3i8x4"))))
    (build-system cargo-build-system)
    (arguments
      `( ;; #:skip-build? #t
        #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/dtolnay/semver")
    (synopsis "Parser and evaluator for Cargo's flavor of Semantic Versioning")
    (description
      "Parser and evaluator for Cargo's flavor of Semantic Versioning")
    (license (list license:expat license:asl2.0))))

(define-public rust-dlv-list-0.3
  (package
    (name "rust-dlv-list")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dlv-list" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0mqj5rdkcjksw3kvjj0nga6rzcpppx0kimjwi527yhifz6kw5206"))))
    (build-system cargo-build-system)
    ;; (arguments `(#:skip-build? #t))
    (home-page "https://github.com/sgodwincs/dlv-list-rs")
    (synopsis "Semi-doubly linked list implemented using a vector")
    (description "Semi-doubly linked list implemented using a vector")
    (license license:expat)))

(define-public rust-ordered-multimap-0.4
  (package
    (name "rust-ordered-multimap")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ordered-multimap" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0l7z0lcmjk1wf4iskxq0wgx32dw1l6fxzrvlin5nzldaq1gnqivv"))))
    (build-system cargo-build-system)
    (arguments
     `( ;; #:skip-build? #t
       #:cargo-inputs
       (("rust-dlv-list" ,rust-dlv-list-0.3)
        ("rust-hashbrown" ,rust-hashbrown-0.11)
        ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/sgodwincs/ordered-multimap-rs")
    (synopsis "Insertion ordered multimap")
    (description "Insertion ordered multimap")
    (license license:expat)))

(define-public rust-rust-ini-0.18
  (package
    (name "rust-rust-ini")
    (version "0.18.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rust-ini" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1px22l3m84v7f46pa3p4bsjykivw8ryq6af8kpkzdd16c11z5mgn"))))
    (build-system cargo-build-system)
    (arguments
     `( ;; #:skip-build? #t
       #:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-ordered-multimap" ,rust-ordered-multimap-0.4)
        ("rust-unicase" ,rust-unicase-2))))
    (home-page "https://github.com/zonyitoo/rust-ini")
    (synopsis "An Ini configuration file parsing library in Rust")
    (description "An Ini configuration file parsing library in Rust")
    (license license:expat)))

(define-public rust-regex-1
  (package
    (name "rust-regex")
    (version "1.5.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "11kjfh41h7i33sskb8i36kl03260rrjw74nb2njhbzr5ddxn848s"))))
    (build-system cargo-build-system)
    (arguments
     `( ;; #:skip-build? #t
       #:cargo-inputs
       (("rust-aho-corasick" ,rust-aho-corasick-0.7)
        ("rust-memchr" ,rust-memchr-2)
        ("rust-regex-syntax" ,rust-regex-syntax-0.6))
       #:cargo-development-inputs
       (("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-quickcheck" ,rust-quickcheck-1)
        ("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/rust-lang/regex")
    (synopsis
      "An implementation of regular expressions for Rust. This implementation uses
finite automata and guarantees linear time matching on all inputs.
")
    (description
      "An implementation of regular expressions for Rust.  This implementation uses
finite automata and guarantees linear time matching on all inputs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-packed-simd-2-0.3
  (package
    (name "rust-packed-simd-2")
    (version "0.3.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "packed_simd_2" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "19jr02hqrjs5x3lb1mp0lhvnbpr8rvs9y7gp1394mi6whvpwzzfy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t                 ; nighlty
       #:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-core-arch" ,rust-core-arch-0.1)
        ("rust-libm" ,rust-libm-0.1)
        ("rust-sleef-sys" ,rust-sleef-sys-0.1))
       #:cargo-development-inputs
       (("rust-paste" ,rust-paste-0.1)
        ("rust-arrayvec" ,rust-arrayvec-0.5))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'unix-cargo-toml
           (lambda _
             (setenv "CARGO_BUILD_RUSTFLAGS"
                     (string-append "--cfg 'target_os=\"linux\"'"))
             (delete-file "Cargo.toml")
             (substitute* "Cargo.toml.orig"
               (("^wasm-bindgen.*") ""))
             (rename-file "Cargo.toml.orig" "Cargo.toml")
             #t)))))
    (home-page "https://github.com/rust-lang/packed_simd")
    (synopsis "Portable Packed SIMD vectors")
    (description "Portable Packed SIMD vectors")
    (license (list license:expat license:asl2.0))))

(define-public rust-rand-0.8
  (package
    (name "rust-rand")
    (version "0.8.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "013l6931nn7gkc23jz5mm3qdhf93jjf0fg64nz2lp4i51qd8vbrl"))))
    (build-system cargo-build-system)
    (arguments
     `( ;; #:skip-build? #t
       #:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-packed-simd-2" ,rust-packed-simd-2-0.3)
        ("rust-rand-chacha" ,rust-rand-chacha-0.3)
        ("rust-rand-core" ,rust-rand-core-0.6)
        ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs
       (("rust-bincode" ,rust-bincode-1)
        ("rust-rand-pcg" ,rust-rand-pcg-0.3))))
    (home-page "https://rust-random.github.io/book")
    (synopsis "Random number generators and other randomness functionality.
")
    (description
      "Random number generators and other randomness functionality.")
    (license (list license:expat license:asl2.0))))

(define-public rust-libc-0.2
  (package
    (name "rust-libc")
    (version "0.2.120")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libc" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "028c71ww2h32ywr92075v0ywb7z562cs8v1y06fr7l2r0zl18p5d"))))
    (build-system cargo-build-system)
    (arguments
     `( ;; #:skip-build? #t
       #:cargo-inputs
       (("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/rust-lang/libc")
    (synopsis "Raw FFI bindings to platform libraries like libc.
")
    (description "Raw FFI bindings to platform libraries like libc.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rand-pcg-0.3
  (package
    (name "rust-rand-pcg")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_pcg" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0gn79wzs5b19iivybwa09wv4lhi4kbcqciasiqqynggnr8cd1jjr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-rand-core" ,rust-rand-core-0.6) ("rust-serde" ,rust-serde-1))
        #:cargo-development-inputs
        (("rust-bincode" ,rust-bincode-1))))
    (home-page "https://rust-random.github.io/book")
    (synopsis "Selected PCG random number generators
")
    (description "Selected PCG random number generators")
    (license (list license:expat license:asl2.0))))

(define-public rust-process-control-3
  (package
    (name "rust-process-control")
    (version "3.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "process_control" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17s7p1rfjpkxgiym6hj1sy3rl7iwf57sj2cbb34ia655cafgmm16"))))
    (build-system cargo-build-system)
    (arguments
     `( ;; #:skip-build? #t
       #:cargo-inputs
       (("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
        ("rust-libc" ,rust-libc-0.2))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'unix-cargo-toml
           (lambda _
             (setenv "CARGO_BUILD_RUSTFLAGS"
                     (string-append "--cfg 'target_os=\"linux\"'"))
             (delete-file "Cargo.toml")
             (substitute* "Cargo.toml.orig"
               (("^windows-sys.*") ""))
             (rename-file "Cargo.toml.orig" "Cargo.toml")
             #t)))))
    (home-page "https://github.com/dylni/process_control")
    (synopsis "Methods for ergonomically running processes with timeouts")
    (description "Methods for ergonomically running processes with timeouts")
    (license (list license:expat license:asl2.0))))

(define-public rust-os-info-3
  (package
    (name "rust-os-info")
    (version "3.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "os_info" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0h6kb5w5hw5b2anjyb45njfmqn0klrcl8bzxcz7pkx2yai6zhg82"))))
    (build-system cargo-build-system)
    (arguments
     `( ;; #:skip-build? #t
       #:cargo-inputs
       (("rust-log" ,rust-log-0.4)
        ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1))))
    (home-page "https://github.com/stanislav-tkach/os_info")
    (synopsis "Detect the operating system type and version.")
    (description "Detect the operating system type and version.")
    (license license:expat)))

(define-public rust-open-2
  (package
    (name "rust-open")
    (version "2.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "open" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0660lsi0rqv23smbm0mm48wjmwnz3869iqlfh8i7fsm0davff4wj"))))
    (build-system cargo-build-system)
    (arguments
     `( ;; #:skip-build? #t
       #:cargo-inputs
       (("rust-pathdiff" ,rust-pathdiff-0.2))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'unix-cargo-toml
           (lambda _
             (setenv "CARGO_BUILD_RUSTFLAGS"
                     (string-append "--cfg 'target_os=\"linux\"'"))
             (delete-file "Cargo.toml")
             (substitute* "Cargo.toml.orig"
               (("^winapi.*") ""))
             (rename-file "Cargo.toml.orig" "Cargo.toml")
             #t)))))
    (home-page "https://github.com/Byron/open-rs")
    (synopsis "Open a path or URL using the program configured on the system")
    (description
      "Open a path or URL using the program configured on the system")
    (license license:expat)))

(define-public rust-thread-id-4
  (package
    (name "rust-thread-id")
    (version "4.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "thread-id" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0zvikdngp0950hi0jgiipr8l36rskk1wk7pc8cd43xr3g5if1psz"))))
    (build-system cargo-build-system)
    (arguments
     `( ;; #:skip-build? #t
       #:cargo-inputs
       (("rust-libc" ,rust-libc-0.2))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'unix-cargo-toml
           (lambda _
             (setenv "CARGO_BUILD_RUSTFLAGS"
                     (string-append "--cfg 'target_os=\"linux\"'"))
             (delete-file "Cargo.toml")
             (substitute* "Cargo.toml.orig"
               (("^winapi.*") "")
               (("^redox_syscall.*") ""))
             (rename-file "Cargo.toml.orig" "Cargo.toml")
             #t)))))
    (home-page "https://github.com/ruuda/thread-id")
    (synopsis "Get a unique thread ID")
    (description "Get a unique thread ID")
    (license (list license:expat license:asl2.0))))

(define-public rust-object-0.27
  (package
    (name "rust-object")
    (version "0.27.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "object" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1ygv9zgi9wz6q5f2z9xn72i0c97jjr1dgj30kbyicdhxk8zivb37"))))
    (build-system cargo-build-system)
    (arguments
     `( ;; #:skip-build? #t
       #:cargo-inputs
       (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
        ("rust-crc32fast" ,rust-crc32fast-1)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-memchr" ,rust-memchr-2)
        ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
        ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
        ("rust-wasmparser" ,rust-wasmparser-0.57))))
    (home-page "https://github.com/gimli-rs/object")
    (synopsis
      "A unified interface for reading and writing object file formats.")
    (description
      "This package provides a unified interface for reading and writing object file
formats.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-gimli-0.26
  (package
    (name "rust-gimli")
    (version "0.26.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gimli" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1m0vi36ypv4gx9gzcw6y456yqnlypizhwlcqrmg6vkwd0lnkgk3q"))))
    (build-system cargo-build-system)
    (arguments
     `( ;; #:skip-build? #t
       #:cargo-inputs
       (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
        ("rust-fallible-iterator" ,rust-fallible-iterator-0.2)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
        ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
        ("rust-stable-deref-trait" ,rust-stable-deref-trait-1))
       #:cargo-development-inputs
       (("rust-crossbeam" ,rust-crossbeam-0.8)
        ("rust-getopts" ,rust-getopts-0.2)
        ("rust-memmap" ,rust-memmap-0.7)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-object" ,rust-object-0.27)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-regex" ,rust-regex-1)
        ("rust-byteorder" ,rust-byteorder-0.5)
        ("rust-test-assembler" ,rust-test-assembler-0.1)
        ("rust-typed-arena" ,rust-typed-arena-2))))
    (home-page "https://github.com/gimli-rs/gimli")
    (synopsis "A library for reading and writing the DWARF debugging format.")
    (description
      "This package provides a library for reading and writing the DWARF debugging
format.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-object-0.27
  (package
    (name "rust-object")
    (version "0.27.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "object" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1ygv9zgi9wz6q5f2z9xn72i0c97jjr1dgj30kbyicdhxk8zivb37"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
         ("rust-crc32fast" ,rust-crc32fast-1)
         ("rust-flate2" ,rust-flate2-1)
         ("rust-indexmap" ,rust-indexmap-1)
         ("rust-memchr" ,rust-memchr-2)
         ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
         ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
         ("rust-wasmparser" ,rust-wasmparser-0.57))))
    (home-page "https://github.com/gimli-rs/object")
    (synopsis
      "A unified interface for reading and writing object file formats.")
    (description
      "This package provides a unified interface for reading and writing object file
formats.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-addr2line-0.17
  (package
    (name "rust-addr2line")
    (version "0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "addr2line" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sw16zqy6w0ar633z69m7lw6gb0k1y7xj3387a8wly43ij5div5r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
                     #:cargo-inputs
                     (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                      ("rust-cpp-demangle" ,rust-cpp-demangle-0.3)
                      ("rust-fallible-iterator" ,rust-fallible-iterator-0.2)
                      ("rust-gimli" ,rust-gimli-0.26)
                      ("rust-object" ,rust-object-0.27)
                      ("rust-rustc-demangle" ,rust-rustc-demangle-0.1)
                      ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                      ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                      ("rust-smallvec" ,rust-smallvec-1))
                     #:cargo-development-inputs
                     (("rust-memmap" ,rust-memmap-0.7)
                      ("rust-clap" ,rust-clap-3)
                      ("rust-backtrace" ,rust-backtrace-0.3)
                      ("rust-findshlibs" ,rust-findshlibs-0.10)
                      ("rust-rustc-test" ,rust-rustc-test-0.3)
                      ("rust-typed-arena" ,rust-typed-arena-2)
                      ;; ("rust-auxiliary" ,rust-clap-3)
                      ;; auxiliary = { path = "tests/auxiliary" }
                      )))
    (home-page "https://github.com/gimli-rs/addr2line")
    (synopsis
     "A cross-platform symbolication library written in Rust, using `gimli`")
    (description
     "This package provides a cross-platform symbolication library written in Rust,
using `gimli`")
    (license (list license:asl2.0 license:expat))))

(define-public rust-backtrace-0.3
  (package
    (name "rust-backtrace")
    (version "0.3.64")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "backtrace" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "07y3z67h9mybdw4l1cjrlqw3ng7h7m4y374d4jmk7ki3h3p1s4jy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-addr2line" ,rust-addr2line-0.17)
        ("rust-cc" ,rust-cc-1)
        ("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-cpp-demangle" ,rust-cpp-demangle-0.3)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-miniz-oxide" ,rust-miniz-oxide-0.4)
        ("rust-object" ,rust-object-0.27)
        ("rust-rustc-demangle" ,rust-rustc-demangle-0.1)
        ("rust-rustc-serialize" ,rust-rustc-serialize-0.3)
        ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs
       (("rust-libloading" ,rust-libloading-0.7)
        ;; dylib-dep = { path = "crates/dylib-dep" }
        )))
    (home-page "https://github.com/rust-lang/backtrace-rs")
    (synopsis
      "A library to acquire a stack trace (backtrace) at runtime in a Rust program.
")
    (description
      "This package provides a library to acquire a stack trace (backtrace) at runtime
in a Rust program.")
    (license (list license:expat license:asl2.0))))

(define-public rust-parking-lot-core-0.9
  (package
    (name "rust-parking-lot-core")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parking_lot_core" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0m49xlpxyw0c65c3011zvgzn2slpviw494816d2a4g8lqh61w518"))))
    (build-system cargo-build-system)
    (arguments
     `( ;; #:skip-build? #t
       #:cargo-inputs
       (("rust-backtrace" ,rust-backtrace-0.3)
        ("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-petgraph" ,rust-petgraph-0.5)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thread-id" ,rust-thread-id-4))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'unix-cargo-toml
           (setenv "CARGO_BUILD_RUSTFLAGS"
                   (string-append "--cfg 'target_os=\"linux\"'"))))))
    (home-page "https://github.com/Amanieu/parking_lot")
    (synopsis
      "An advanced API for creating custom synchronization primitives.")
    (description
      "An advanced API for creating custom synchronization primitives.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-lock-api-0.4
  (package
    (name "rust-lock-api")
    (version "0.4.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lock_api" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0frbbqqiwngg33xrc69xagi4rqqk62msllr7z95mlbjaxzbkv548"))))
    (build-system cargo-build-system)
    (arguments
     `( ;; #:skip-build? #t
       #:cargo-inputs
       (("rust-owning-ref" ,rust-owning-ref-0.4)
        ("rust-scopeguard" ,rust-scopeguard-1)
        ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/Amanieu/parking_lot")
    (synopsis
      "Wrappers to create fully-featured Mutex and RwLock types. Compatible with no_std.")
    (description
      "Wrappers to create fully-featured Mutex and RwLock types.  Compatible with
no_std.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-libtest-mimic-0.3
  (package
    (name "rust-libtest-mimic")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libtest-mimic" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bp2jllwpciljr14g6s9bk4835g46kszgrjwi66vxxsk3ynbi9q8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-crossbeam-channel" ,rust-crossbeam-channel-0.4)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-structopt" ,rust-structopt-0.3)
        ("rust-termcolor" ,rust-termcolor-1))))
    (home-page "https://github.com/LukasKalbertodt/libtest-mimic")
    (synopsis
     "Write your own test harness that looks and behaves like the built-in test harness used by `rustc --test`
")
    (description
     "Write your own test harness that looks and behaves like the built-in test
harness used by `rustc --test`")
    (license (list license:expat license:asl2.0))))

(define-public rust-parking-lot-0.12
  (package
    (name "rust-parking-lot")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "parking_lot" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0n7gp0cnfghglc370cxhawwfijvhj3wrjh8gdi8c06m6jcjfrxc7"))))
    (build-system cargo-build-system)
    (arguments
     `( ;; #:skip-build? #t
       #:cargo-inputs
       (("rust-lock-api" ,rust-lock-api-0.4)
        ("rust-parking-lot-core" ,rust-parking-lot-core-0.9))
       #:cargo-development-inputs
       (("rust-bincode" ,rust-bincode-1)
        ("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/Amanieu/parking_lot")
    (synopsis
     "More compact and efficient implementations of the standard synchronization primitives.")
    (description
     "More compact and efficient implementations of the standard synchronization
primitives.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-backtrace-0.3
  (package
    (name "rust-backtrace")
    (version "0.3.64")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "backtrace" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "07y3z67h9mybdw4l1cjrlqw3ng7h7m4y374d4jmk7ki3h3p1s4jy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-addr2line" ,rust-addr2line-0.17)
        ("rust-cc" ,rust-cc-1)
        ("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-cpp-demangle" ,rust-cpp-demangle-0.3)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-miniz-oxide" ,rust-miniz-oxide-0.4)
        ("rust-object" ,rust-object-0.27)
        ("rust-rustc-demangle" ,rust-rustc-demangle-0.1)
        ("rust-rustc-serialize" ,rust-rustc-serialize-0.3)
        ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs
       (("rust-libloading" ,rust-libloading-0.7)
        ;; dylib-dep = { path = "crates/dylib-dep" }
        )))
    (home-page "https://github.com/rust-lang/backtrace-rs")
    (synopsis
      "A library to acquire a stack trace (backtrace) at runtime in a Rust program.
")
    (description
      "This package provides a library to acquire a stack trace (backtrace) at runtime
in a Rust program.")
    (license (list license:expat license:asl2.0))))

(define-public rust-thread-id-4
  (package
    (name "rust-thread-id")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "thread-id" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zvikdngp0950hi0jgiipr8l36rskk1wk7pc8cd43xr3g5if1psz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-redox-syscall" ,rust-redox-syscall-0.2)
        ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/ruuda/thread-id")
    (synopsis "Get a unique thread ID")
    (description "Get a unique thread ID")
    (license (list license:expat license:asl2.0))))

(define-public rust-parking-lot-core-0.9
  (package
   (name "rust-parking-lot-core")
   (version "0.9.2")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "parking_lot_core" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0dxbggwcbbds5zys68mrl7py5w42aph1ihis8fq008l2dix6cpwr"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs
      (("rust-backtrace" ,rust-backtrace-0.3)
       ("rust-cfg-if" ,rust-cfg-if-1)
       ("rust-libc" ,rust-libc-0.2)
       ("rust-petgraph" ,rust-petgraph-0.6)
       ("rust-redox-syscall" ,rust-redox-syscall-0.2)
       ("rust-smallvec" ,rust-smallvec-1)
       ("rust-thread-id" ,rust-thread-id-4)
       ("rust-windows-sys" ,rust-windows-sys-0.34))))
   (home-page "https://github.com/Amanieu/parking_lot")
   (synopsis
    "An advanced API for creating custom synchronization primitives.")
   (description
    "An advanced API for creating custom synchronization primitives.")
   (license (list license:expat license:asl2.0))))

(define-public rust-windows-x86-64-msvc-0.34
  (package
   (name "rust-windows-x86-64-msvc")
   (version "0.34.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "windows_x86_64_msvc" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1scxv2b9id3gj6bvpzdax496lng6x8bnm3gqx8fx068qqb63i5fi"))))
   (build-system cargo-build-system)
   (arguments `(#:skip-build? #t))
   (home-page "https://github.com/microsoft/windows-rs")
   (synopsis "Code gen support for the windows crate")
   (description "Code gen support for the windows crate")
   (license (list license:expat license:asl2.0))))

(define-public rust-windows-x86-64-gnu-0.34
  (package
   (name "rust-windows-x86-64-gnu")
   (version "0.34.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "windows_x86_64_gnu" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1x71512gic645ri51y6ivw1wb72h38agrvqrdlsqvvi7wbm6vkng"))))
   (build-system cargo-build-system)
   (arguments `(#:skip-build? #t))
   (home-page "https://github.com/microsoft/windows-rs")
   (synopsis "Code gen support for the windows crate")
   (description "Code gen support for the windows crate")
   (license (list license:expat license:asl2.0))))

(define-public rust-windows-i686-msvc-0.34
  (package
   (name "rust-windows-i686-msvc")
   (version "0.34.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "windows_i686_msvc" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0mk92rzdvjks01v8d5dkh1yp9syf9f0khkf168im4lq4lwmx7ncw"))))
   (build-system cargo-build-system)
   (arguments `(#:skip-build? #t))
   (home-page "https://github.com/microsoft/windows-rs")
   (synopsis "Code gen support for the windows crate")
   (description "Code gen support for the windows crate")
   (license (list license:expat license:asl2.0))))

(define-public rust-windows-i686-gnu-0.34
  (package
   (name "rust-windows-i686-gnu")
   (version "0.34.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "windows_i686_gnu" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1vc8hr4hm1x89h997gwfq5q9kj1j5gj4pxdlv4lr3dxdb7kzsr15"))))
   (build-system cargo-build-system)
   (arguments `(#:skip-build? #t))
   (home-page "https://github.com/microsoft/windows-rs")
   (synopsis "Code gen support for the windows crate")
   (description "Code gen support for the windows crate")
   (license (list license:expat license:asl2.0))))

(define-public rust-windows-aarch64-msvc-0.34
  (package
   (name "rust-windows-aarch64-msvc")
   (version "0.34.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "windows_aarch64_msvc" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "07c8vcqcvkmm2d921488w05dyjl047jc03xddyszy6hj83kzpkqp"))))
   (build-system cargo-build-system)
   (arguments `(#:skip-build? #t))
   (home-page "https://github.com/microsoft/windows-rs")
   (synopsis "Code gen support for the windows crate")
   (description "Code gen support for the windows crate")
   (license (list license:expat license:asl2.0))))

(define-public rust-windows-sys-0.34
  (package
   (name "rust-windows-sys")
   (version "0.34.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "windows-sys" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0988yzc9j7ps1ajf4jlh8w0d20saz7c64ky1b82c0m5snj6dgkas"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs
      (("rust-windows-aarch64-msvc" ,rust-windows-aarch64-msvc-0.34)
       ("rust-windows-i686-gnu" ,rust-windows-i686-gnu-0.34)
       ("rust-windows-i686-msvc" ,rust-windows-i686-msvc-0.34)
       ("rust-windows-x86-64-gnu" ,rust-windows-x86-64-gnu-0.34)
       ("rust-windows-x86-64-msvc" ,rust-windows-x86-64-msvc-0.34))))
   (home-page "https://github.com/microsoft/windows-rs")
   (synopsis "Rust for Windows")
   (description "Rust for Windows")
   (license (list license:expat license:asl2.0))))

(define-public rust-thiserror-impl-1
  (package
   (name "rust-thiserror-impl")
   (version "1.0.30")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "thiserror-impl" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0jviwmvx6wzawsj6c9msic7h419wmsbjagl9dzhpydkzc8zzscma"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2-1)
       ("rust-quote" ,rust-quote-1)
       ("rust-syn" ,rust-syn-1))))
   (home-page "https://github.com/dtolnay/thiserror")
   (synopsis "Implementation detail of the `thiserror` crate")
   (description "Implementation detail of the `thiserror` crate")
   (license (list license:expat license:asl2.0))))

(define-public rust-thiserror-1
  (package
   (name "rust-thiserror")
   (version "1.0.30")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "thiserror" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "05y4wm29ck8flwq5k1q6nhwh00a3b30cz3xr0qvnbwad5vjsnjw5"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs
      (("rust-thiserror-impl" ,rust-thiserror-impl-1))
      #:cargo-development-inputs
      (("rust-anyhow" ,rust-anyhow-1)
       ("rust-ref-cast" ,rust-ref-cast-1)
       ("rust-rustversion" ,rust-rustversion-1)
       ("rust-trybuild" ,rust-trybuild-1))))
   (home-page "https://github.com/dtolnay/thiserror")
   (synopsis "derive(Error)")
   (description "derive(Error)")
   (license (list license:expat license:asl2.0))))

(define-public rust-swayipc-types-1
  (package
   (name "rust-swayipc-types")
   (version "1.1.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "swayipc-types" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "11p2jj081f7bw2rc6xqvw3y8bdraqqrmm3vsvq5mx7ds00r35jiq"))))
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
   (version "3.0.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "swayipc" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1v6arq79b72wm938yvb72j8np04n4ia9r78idg2ffcczp8mpxk20"))))
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
   (version "0.3.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "rt-format" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1bmxn40aaflxgrz5pdjdkk244aj293g65vyrg0dbnb65gwizyglm"))))
   (build-system cargo-build-system)
   (arguments
    `(#:skip-build?
      #t
      #:cargo-inputs
      (("rust-lazy-static" ,rust-lazy-static-1)
       ("rust-regex" ,rust-regex-1))))
   (home-page "https://github.com/vstojkovic/rt-format")
   (synopsis "Fully-runtime equivalent of the format! macro")
   (description "Fully-runtime equivalent of the format! macro")
   (license license:asl2.0)))

(define-public rust-unic-emoji-char-0.9
  (package
   (name "rust-unic-emoji-char")
   (version "0.9.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "unic-emoji-char" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0ka9fr7s6lv0z43r9xphg9injn35pfxf9g9q18ki0wl9d0g241qb"))))
   (build-system cargo-build-system)
   (arguments
    `(#:skip-build?
      #t
      #:cargo-inputs
      (("rust-unic-char-property" ,rust-unic-char-property-0.9)
       ("rust-unic-char-range" ,rust-unic-char-range-0.9)
       ("rust-unic-ucd-version" ,rust-unic-ucd-version-0.9))))
   (home-page "https://github.com/open-i18n/rust-unic/")
   (synopsis
    "UNIC â\x80\x94 Unicode Emoji â\x80\x94 Emoji Character Properties")
   (description
    "UNIC â\x80\x94 Unicode Emoji â\x80\x94 Emoji Character Properties")
   (license (list license:expat license:asl2.0))))

(define-public rust-unic-emoji-char-0.9
  (package
   (name "rust-unic-emoji-char")
   (version "0.9.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "unic-emoji-char" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0ka9fr7s6lv0z43r9xphg9injn35pfxf9g9q18ki0wl9d0g241qb"))))
   (build-system cargo-build-system)
   (arguments
    `(#:skip-build?
      #t
      #:cargo-inputs
      (("rust-unic-char-property" ,rust-unic-char-property-0.9)
       ("rust-unic-char-range" ,rust-unic-char-range-0.9)
       ("rust-unic-ucd-version" ,rust-unic-ucd-version-0.9))))
   (home-page "https://github.com/open-i18n/rust-unic/")
   (synopsis
    "UNIC â\x80\x94 Unicode Emoji â\x80\x94 Emoji Character Properties")
   (description
    "UNIC â\x80\x94 Unicode Emoji â\x80\x94 Emoji Character Properties")
   (license (list license:expat license:asl2.0))))

(define-public rust-termion-1
  (package
   (name "rust-termion")
   (version "1.5.6")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "termion" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0zk023f0zkws358ll399cawvwdnd0wg8wad4g61kz766xbi8aw87"))))
   (build-system cargo-build-system)
   (arguments
    `(#:skip-build?
      #t
      #:cargo-inputs
      (("rust-libc" ,rust-libc-0.2)
       ("rust-numtoa" ,rust-numtoa-0.1)
       ("rust-redox-syscall" ,rust-redox-syscall-0.2)
       ("rust-redox-termios" ,rust-redox-termios-0.1))))
   (home-page "https://gitlab.redox-os.org/redox-os/termion")
   (synopsis "A bindless library for manipulating terminals.")
   (description
    "This package provides a bindless library for manipulating terminals.")
   (license license:expat)))

(define-public rust-toml-edit-0.10
  (package
   (name "rust-toml-edit")
   (version "0.10.1")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "toml_edit" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1a6zw9rd8m0qbal6a3dyq1mvyb9hv16ydsrvsrk6yzwgqh0yp0i7"))))
   (build-system cargo-build-system)
   (arguments
    `(#:skip-build?
      #t
      #:cargo-inputs
      (("rust-combine" ,rust-combine-4)
       ("rust-indexmap" ,rust-indexmap-1)
       ("rust-itertools" ,rust-itertools-0.10)
       ("rust-kstring" ,rust-kstring-1)
       ("rust-serde" ,rust-serde-1))))
   (home-page "https://github.com/ordian/toml_edit")
   (synopsis "Yet another format-preserving TOML parser.")
   (description "Yet another format-preserving TOML parser.")
   (license (list license:expat license:asl2.0))))

(define-public rust-kstring-2
  (package
   (name "rust-kstring")
   (version "2.0.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "kstring" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0isp7kmk4q0qxpcd877q77ykgb3ryfbmj18djmnwv8c210sncc7c"))))
   (build-system cargo-build-system)
   (arguments
    `(#:skip-build?
      #t
      #:cargo-inputs
      (("rust-document-features" ,rust-document-features-0.2)
       ("rust-serde" ,rust-serde-1)
       ("rust-static-assertions" ,rust-static-assertions-1))))
   (home-page "https://github.com/cobalt-org/kstring")
   (synopsis "Key String: optimized for map keys")
   (description "Key String: optimized for map keys")
   (license (list license:expat license:asl2.0))))

(define-public rust-toml-edit-0.14
  (package
   (name "rust-toml-edit")
   (version "0.14.4")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "toml_edit" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "07xmklks4ldf8r9ban47zvq2s0csq43ja0dcjs43yi7j8ip2axjk"))))
   (build-system cargo-build-system)
   (arguments
    `(#:skip-build?
      #t
      #:cargo-inputs
      (("rust-combine" ,rust-combine-4)
       ("rust-indexmap" ,rust-indexmap-1)
       ("rust-itertools" ,rust-itertools-0.10)
       ("rust-kstring" ,rust-kstring-2)
       ("rust-serde" ,rust-serde-1))))
   (home-page "https://github.com/ordian/toml_edit")
   (synopsis "Yet another format-preserving TOML parser.")
   (description "Yet another format-preserving TOML parser.")
   (license (list license:expat license:asl2.0))))

(define-public rust-trycmd-0.13
  (package
   (name "rust-trycmd")
   (version "0.13.4")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "trycmd" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "04wzh907rkxac5kxlai0s630qh9z122w2m1s2x14d46c4r8iid7z"))))
   (build-system cargo-build-system)
   (arguments
    `(#:skip-build?
      #t
      #:cargo-inputs
      (("rust-escargot" ,rust-escargot-0.5)
       ("rust-glob" ,rust-glob-0.3)
       ("rust-humantime" ,rust-humantime-2)
       ("rust-humantime-serde" ,rust-humantime-serde-1)
       ("rust-rayon" ,rust-rayon-1)
       ("rust-schemars" ,rust-schemars-0.8)
       ("rust-serde" ,rust-serde-1)
       ("rust-serde-json" ,rust-serde-json-1)
       ("rust-shlex" ,rust-shlex-1)
       ("rust-snapbox" ,rust-snapbox-0.2)
       ("rust-toml-edit" ,rust-toml-edit-0.14))))
   (home-page "https://github.com/assert-rs/trycmd")
   (synopsis "Snapshot testing for a herd of CLI tests")
   (description "Snapshot testing for a herd of CLI tests")
   (license (list license:expat license:asl2.0))))

(define-public rust-snapbox-macros-0.2
  (package
   (name "rust-snapbox-macros")
   (version "0.2.1")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "snapbox-macros" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0c79lnjcs9yp62y665swv5y5y6088qc256bfr3s7xcnb0izfl7f0"))))
   (build-system cargo-build-system)
   (arguments `(#:skip-build? #t))
   (home-page "https://github.com/assert-rs/trycmd/tree/main/crates/snapbox")
   (synopsis "Snapshot testing toolbox")
   (description "Snapshot testing toolbox")
   (license (list license:expat license:asl2.0))))

(define-public rust-similar-2
  (package
    (name "rust-similar")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "similar" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1lw33na01r35h09s47jqhjgz3m29wapl20f6ybsla5d1cfgrf91f"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bstr" ,rust-bstr-0.2)
         ("rust-serde" ,rust-serde-1)
         ("rust-unicode-segmentation" ,rust-unicode-segmentation-1))
        #:cargo-development-inputs
        (("rust-console" ,rust-console-0.14)
         ("rust-insta" ,rust-insta-1)
         ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/mitsuhiko/similar")
    (synopsis "A diff library for Rust")
    (description "This package provides a diff library for Rust")
    (license license:asl2.0)))

(define-public rust-snapbox-0.2
  (package
   (name "rust-snapbox")
   (version "0.2.10")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "snapbox" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "14zxmsi4k9a9vgp9vs1q62ff1k57p26rwp5xs6f9bdijl9fisykn"))))
   (build-system cargo-build-system)
   (arguments
    `(#:skip-build?
      #t
      #:cargo-inputs
      (("rust-backtrace" ,rust-backtrace-0.3)
       ("rust-concolor" ,rust-concolor-0.0.8)
       ("rust-content-inspector" ,rust-content-inspector-0.2)
       ("rust-document-features" ,rust-document-features-0.2)
       ("rust-dunce" ,rust-dunce-1)
       ("rust-filetime" ,rust-filetime-0.2)
       ("rust-ignore" ,rust-ignore-0.4)
       ("rust-libtest-mimic" ,rust-libtest-mimic-0.3)
       ("rust-normalize-line-endings" ,rust-normalize-line-endings-0.3)
       ("rust-os-pipe" ,rust-os-pipe-1)
       ("rust-similar" ,rust-similar-2)
       ("rust-snapbox-macros" ,rust-snapbox-macros-0.2)
       ("rust-tempfile" ,rust-tempfile-3)
       ("rust-wait-timeout" ,rust-wait-timeout-0.2)
       ("rust-walkdir" ,rust-walkdir-2)
       ("rust-yansi" ,rust-yansi-0.5))))
   (home-page "https://github.com/assert-rs/trycmd/tree/main/crates/snapbox")
   (synopsis "Snapshot testing toolbox")
   (description "Snapshot testing toolbox")
   (license (list license:expat license:asl2.0))))

(define-public rust-clap-lex-0.2
  (package
   (name "rust-clap-lex")
   (version "0.2.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "clap_lex" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "04wjgd1d3rxsng70rczfzhc7lj87hmwzznhs1dp5xb9d27qkaz53"))))
   (build-system cargo-build-system)
   (arguments
    `(#:skip-build?
      #t
      #:cargo-inputs
      (("rust-os-str-bytes" ,rust-os-str-bytes-6))))
   (home-page "https://github.com/clap-rs/clap/tree/master/clap_lex")
   (synopsis "Minimal, flexible command line parser")
   (description "Minimal, flexible command line parser")
   (license (list license:expat license:asl2.0))))

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

(define-public rust-once-cell-1
  (package
   (name "rust-once-cell")
   (version "1.10.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "once_cell" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1fgclb93az22gq5lmqsm84kilx1p1xpij559bmvx2mn1x8vy1ww7"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs
      (("rust-atomic-polyfill" ,rust-atomic-polyfill-0.1)
       ("rust-parking-lot" ,rust-parking-lot-0.12))
      #:cargo-development-inputs
      (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.7)
       ("rust-lazy-static" ,rust-lazy-static-1)
       ("rust-regex" ,rust-regex-1))))
   (home-page "https://github.com/matklad/once_cell")
   (synopsis "Single assignment cells and lazy values.")
   (description "Single assignment cells and lazy values.")
   (license (list license:expat license:asl2.0))))

(define-public rust-regex-1
  (package
   (name "rust-regex")
   (version "1.5.5")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "regex" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "11kjfh41h7i33sskb8i36kl03260rrjw74nb2njhbzr5ddxn848s"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs
      (("rust-aho-corasick" ,rust-aho-corasick-0.7)
       ("rust-memchr" ,rust-memchr-2)
       ("rust-regex-syntax" ,rust-regex-syntax-0.6))
      #:cargo-development-inputs
      (("rust-lazy-static" ,rust-lazy-static-1)
       ("rust-quickcheck" ,rust-quickcheck-1)
       ("rust-rand" ,rust-rand-0.8))))
   (home-page "https://github.com/rust-lang/regex")
   (synopsis
    "An implementation of regular expressions for Rust. This implementation uses
finite automata and guarantees linear time matching on all inputs.
")
   (description
    "An implementation of regular expressions for Rust.  This implementation uses
finite automata and guarantees linear time matching on all inputs.")
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

swayr
