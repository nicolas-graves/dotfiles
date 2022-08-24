(define-module (packages restartd)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages))

(define-public restartd
  (let* ((commit "7044125ac55056f2663536f7137170edf92ebd75")
         (revision "1.1"))
    (package
      (name "restartd")
      (version (git-version "0.2.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ajraymond/restartd")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1m1np00b4zvvwx63gzysbi38i5vj1jsjvh2s0p9czl6dzyz582z0"))
         (patches
          (list
           (origin
             (method url-fetch)
             (uri
              (string-append "https://patch-diff.githubusercontent.com/raw"
                             "/ajraymond/restartd/pull/7.patch"))
             (sha256
              (base32
               "0fk33af8sgrgxibmkyjlv3j8jikgbp4mkj84yamvhv38ic6x2rw6")))))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f ; no tests
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'ensure-compilation
              (lambda _
                (substitute* "Makefile"
                  (("CC \\?= gcc") "CC = gcc"))))
            (delete  'configure)
            (replace 'install
              (lambda _
                (install-file "restartd.conf" (string-append #$output "/etc"))
                (install-file "restartd" (string-append #$output "/sbin"))
                (install-file "restartd.8"
                              (string-append #$output "/share/man/man8"))
                (mkdir-p (string-append #$output "/share/man/fr/man8"))
                (copy-file
                 "restartd.fr.8"
                 (string-append #$output "/share/man/fr/man8/restartd.8")))))))
      (home-page "https://launchpad.net/debian/+source/restartd")
      (synopsis "Daemon for restarting processes")
      (description "This package provides a daemon for checking running and not
running processes.  It reads the /proc directory every n seconds and does a
POSIX regexp on the process names.  You can execute a script or a program if
the process is or is not running.")
      (license license:gpl2))))
