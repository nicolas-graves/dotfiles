(define-module (packages vnstat)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build utils)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages check))

(define-public vnstat
  (package
   (name "vnstat")
   (version "2.9")
   (source
    (origin
      (method url-fetch)
      (uri (string-append "https://humdi.net/vnstat/vnstat-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1iwxmnpabfljvyng7c8k3z83yw1687i66z5s1980c5x9vrsi98hi"))))
   (build-system gnu-build-system)
   (inputs (list sqlite))
   (native-inputs (list pkg-config check))
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-before 'check 'disable-id-tests
          (lambda _
            (substitute*
                '("Makefile" "tests/vnstat_tests.c")
              (("tests/id_tests.c \\$") "\\")
              (("tests/id_tests.h h") "h")
              (("^.*id_tests.*$") "")))))))
   (home-page "https://humdi.net/vnstat/")
   (synopsis "a network traffic monitor for Linux and BSD")
   (description "vnStat is a console-based network traffic monitor for Linux
and BSD that keeps a log of network traffic for the selected interface(s). It
uses the network interface statistics provided by the kernel as information
source. This means that vnStat won't actually be sniffing any traffic and also
ensures light use of system resources regardless of network traffic rate.")
   (license license:gpl2)))
