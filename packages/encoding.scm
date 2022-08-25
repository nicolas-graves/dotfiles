(define-module (packages encoding)
  #:use-module (guix build-system perl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages perl-check))

(define-public perl-encode-imaputf7
  (package
    (name "perl-encode-imaputf7")
    (version "1.05")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/P/PM/PMAKHOLM/Encode-IMAPUTF7-"
             version ".tar.gz"))
       (sha256
        (base32 "1q9pgjckjxz0qfwaqmzm1dh1y09819vi6vf1sglcz0vlqgfha0s7"))))
    (build-system perl-build-system)
    (propagated-inputs (list perl-test-nowarnings))
    (home-page "https://metacpan.org/release/Encode-IMAPUTF7")
    (synopsis "Modification of UTF-7 encoding for IMAP")
    (description "IMAP mailbox names are encoded in a modified UTF7 when names
contains international characters outside of the printable ASCII range.  The
modified UTF-7 encoding is defined in RFC2060 (section 5.1.3).  This package
can encode and decode mailbox names according to this convention.")
    (license license:perl-license)))

perl-encode-imaputf7
