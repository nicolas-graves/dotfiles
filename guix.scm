(use-modules (guix gexp)
             (guix packages)
             (guix build-system trivial)
             (gnu packages base))

(package
  (name "config")
  (version "0.0.1")
  (source (local-file "."))
  (build-system trivial-build-system)
  (native-inputs
   `(("make" ,gnu-make)))
  (home-page "")
  (synopsis "")
  (description "")
  (license #f))
