(define-module (home packages emacs)
  #:use-module (gnu packages)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages emacs)
  #:use-module (rde packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guixrus packages emacs)

  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) #:prefix license:))

(define build-emacs emacs-next-pgtk-latest)

(define-public emacs-org-roam-ui
  (let ((commit "9474a254390b1e42488a1801fed5826b32a8030b")
        (revision "0"))
    (package
      (name "emacs-org-roam-ui")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/org-roam/org-roam-ui")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0v54vxh95izch74wz2dl2dkdqicbvshra55l6qvd4xl5qmfhpjdc"))))
      (build-system emacs-build-system)
      (arguments
       `(#:include (cons "^out" %default-include)
                   #:emacs ,build-emacs))
      (propagated-inputs
       (list emacs-org-roam emacs-websocket emacs-simple-httpd emacs-f))
      (home-page "https://github.com/org-roam/org-roam-ui")
      (synopsis "A graphical frontend for your org-roam Zettelkasten")
      (description " Org-Roam-UI is a frontend for exploring and interacting
with your @code{org-roam} notes. It is meant a successor of
@code{org-roam-server} that extends functionality of org-roam with a Web app
that runs side-by-side with Emacs.")
      (license license:gpl3+))))

(define-public emacs-ol-notmuch
  (package
    (name "emacs-ol-notmuch")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://stable.melpa.org/packages/ol-notmuch-"
             version
             ".el"))
       (sha256
        (base32 "1jk2y0wxbyw517wy35snqwjyzqfc11z5hxvn4sbyynprfqgj16h3"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-notmuch emacs-org))
    (home-page "https://git.sr.ht/~tarsius/ol-notmuch")
    (synopsis "Links to notmuch messages")
    (description
     "This file implements links to notmuch messages and \"searches\".  A search is a
query to be performed by notmuch; it is the equivalent to folders in other mail
clients.  Similarly, mails are referred to by a query, so both a link can refer
to several mails.

Links have one the following form notmuch:<search terms> notmuch-search:<search
terms>.

The first form open the queries in notmuch-show mode, whereas the second link
open it in notmuch-search mode.  Note that queries are performed at the time the
link is opened, and the result may be different from when the link was stored.")
  (license license:gpl3+)))

(define-public emacs-org-babel-eval-in-repl
  (package
    (name "emacs-org-babel-eval-in-repl")
    (version "1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://stable.melpa.org/packages/org-babel-eval-in-repl-"
             version
             ".tar"))
       (sha256
        (base32 "0bdnps6m3kcvsagz8cfm3kf2rvxzl2p252pfggwbdbl43kzvl35h"))
       (modules '((guix build utils)))
       ;; Remove matlab
       (snippet
        '(begin (delete-file "eval-in-repl-matlab.el")))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-eval-in-repl emacs-ess))
    (home-page "https://github.com/diadochos/org-babel-eval-in-repl")
    (synopsis "Eval org-mode babel code blocks in various REPLs.")
    (description
     "This package allows you to execute org-mode (babel) source code blocks with
eval-in-repl. It features async execution (because it uses an external
process!) and babel execution without the output written in the buffer (Less
visual distraction! Output is reproducible as long as the code is saved). ")
    (license license:expat)))

(define-public emacs-super-save
  (package
    (name "emacs-super-save")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://stable.melpa.org/packages/super-save-" version
                    ".el"))
              (sha256
               (base32
                "12znjlpk3b3bkd1cvi922r5nn9r9rxahvmcysrdvdv2ckwdff5ch"))))
    (build-system emacs-build-system)
    (arguments `(#:emacs ,build-emacs))
    (home-page "https://github.com/bbatsov/super-save")
    (synopsis "Auto-save buffers, based on your activity.")
    (description "super-save saves buffers when they lose focus.")
    (license license:gpl3)))

(define-public emacs-consult-bibtex
  (let ((commit "2f870d6eefb54a53216d90a82cff0ff97ba54725")
        (revision "0"))
    (package
      (name "emacs-consult-bibtex")
      (version (git-version "0" revision commit))

      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/mohkale/consult-bibtex")
                      (commit commit)))
                (sha256
                 (base32
                  "0jxjvpg3j8zk77sfhyy27fd1zbj2zz7xayavan3hhj4853q92kwq"))))
      (build-system emacs-build-system)
      (arguments `(#:emacs ,build-emacs))
      (inputs (list emacs-org-roam-bibtex emacs-consult emacs-embark))
      (home-page "https://github.com/mohkale/consult-bibtex")
      (synopsis "A read interface for bibtex completion using consult.")
      (description "A read interface for bibtex completion using consult.")
      (license license:gpl3))))
