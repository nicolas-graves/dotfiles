(define-module (packages snapper)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages web)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages tls))

(define-public snapper
  (package
    (name "snapper")
    (version "0.10.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/openSUSE/snapper")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x9anracaa19yqkc0x8wangrkdrx01kdy07c55lvlqrjyimfm4ih"))
       (modules '((guix build utils)))
       (snippet '(begin
                   (delete-file-recursively "dists")
                   (delete-file-recursively "zypp-plugin")
                   (substitute* '("configure.ac" "doc/Makefile.am")
                     ((".*dists.*") "")
                     ((".*zypp-plugin.*") ""))
                   (substitute* "Makefile.am"
                     (("zypp-plugin") ""))))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relative-file-locations
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (substitute* (list "scripts/Makefile.am" "data/Makefile.am")
                 (("/usr/share") (string-append out "/share"))
                 (("/usr/lib") (string-append out "/lib"))
                 (("/etc/") (string-append out "/etc/"))))
               (substitute* "client/Makefile.am"
                 (("/usr/lib") "@libdir@")))))))
    (home-page "https://snapper.io")
    (native-inputs (list autoconf automake libtool pkg-config dbus))
    (inputs
    `(("btrfs" ,btrfs-progs)
      ("e2fs" ,e2fsprogs)
      ("libmount" ,util-linux "lib")
      ("dbus" ,dbus)
      ("libxml" ,libxml2)
      ("json-c" ,json-c)
      ("libacl" ,acl)
      ("boost" ,boost)
      ("libxslt" ,libxslt)
      ("docbook-xsl" ,docbook-xsl)
      ("gettext" ,gettext-minimal)
      ("pam" ,linux-pam)
      ("ncurses" ,ncurses/tinfo)))
    (synopsis "Manage filesystem snapshots and allow undo of system
modifications")
    (description "\
This package provides Snapper, a tool that helps with managing
snapshots of Btrfs subvolumes and thin-provisioned LVM volumes.  It
can create and compare snapshots, revert between snapshots, and
supports automatic snapshots timelines.")
    (license license:gpl2)))
