(define-module (packages emacs)
  #:use-module (gnu packages)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages emacs-xyz)

  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-biblio
  (let* ((commit "72ddab044f82c0f60cbba1b870e3a4c6134145f8")
         (revision "0"))
    (package
      (name "emacs-biblio")
      (version (git-version "0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/nicolas-graves/biblio.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0nw3y9kkaq1f91bsff9g69hzmgsrvizs0d1v1ji0r2fs5zj7xn2i"))))
      (build-system emacs-build-system)
      (propagated-inputs
       (list emacs-dash emacs-let-alist))
      (home-page "https://github.com/cpitclaudel/biblio.el")
      (synopsis "Browse and import bibliographic references")
      (description "This package provides an extensible Emacs package for
browsing and fetching references.

@file{biblio.el} makes it easy to browse and gather bibliographic references
and publications from various sources, by keywords or by DOI.  References are
automatically fetched from well-curated sources, and formatted as BibTeX.")
      (license license:gpl3+))))
