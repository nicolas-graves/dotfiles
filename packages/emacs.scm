(define-module (packages emacs)
  #:use-module (gnu packages)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages erlang)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages version-control)
  #:use-module (rde packages emacs)
  #:use-module (gnu packages emacs-xyz)

  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-js-comint
  (package
    (name "emacs-js-comint")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://stable.melpa.org/packages/js-comint-"
             version
             ".el"))
       (sha256
        (base32 "1qin0hclm3ly62nl5ddiim64bcd2k74b1yqsqqc61cf9k2q8k287"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/redguardtoo/js-comint")
    (synopsis "JavaScript interpreter in window.")
    (description
     "This program is a comint mode for Emacs which allows you to run a compatible
javascript repl like Node.js/Spidermonkey/Rhino inside Emacs. It also defines a
few functions for sending javascript input to it quickly.")
    (license license:gpl3+)))

(define-public emacs-alchemist
  (package
    (name "emacs-alchemist")
    (version "1.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://stable.melpa.org/packages/alchemist-"
             version
             ".tar"))
       (sha256
        (base32 "0ik5zhpsxl6ch7kkjjcvr65hdlgqcxm1ywblavwkszsy2kc15wvj"))))
    (build-system emacs-build-system)
    (propagated-inputs
     (list emacs-elixir-mode emacs-dash emacs-company emacs-pkg-info))
    (home-page "http://www.github.com/tonini/alchemist.el")
  (synopsis "Elixir tooling integration into Emacs")
  (description
   "This package brings you all the Elixir tooling and power inside your Emacs
editor.

 Alchemist comes with a bunch of features, which are:

   * Mix integration
   * Compile & Execution of Elixir code
   * Inline code evaluation
   * Inline macro expanding
   * Documentation lookup
   * Definition lookup
   * Powerful IEx integration
   * Smart code completion
   * Elixir project management
   * Phoenix support")
  (license license:gpl3+)))

(define-public emacs-eval-in-repl
  (package
    (name "emacs-eval-in-repl")
    (version "0.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://raw.githubusercontent.com/kaz-yos/eval-in-repl/"
             version "/eval-in-repl.el"))
       (sha256
        (base32
         "15k2ks034hq2dmm8n70xl7f4cdw57zqb36s871j7kycpkblclg3n"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-dash emacs-paredit emacs-ace-window))
    (home-page "https://github.com/kaz-yos/eval-in-repl")
    (synopsis "Consistent evaluation interface for various REPLs for emacs")
    (description"\
This package provides a consistent ESS-like evaluation interface for
various REPLs.  Emacs Speaks Statistics (ESS) package has a nice function
called @code{ess-eval-region-or-line-and-step}, which is assigned to
C-RET.  This function sends a line or a selected region to the corresponding
shell (R, Julia, Stata, etc) visibly.  It also start up a shell if there is
none.

This package along with REPL/shell specific packages implement similar work flow
for various REPLs.

This package alone is not functional.  It also requires the following packages
depending on your needs :
@itemize
@item eval-in-repl-ielm       for Emacs Lisp
@item eval-in-repl-cider      for Clojure
@item eval-in-repl-slime      for Common Lisp
@item eval-in-repl-geiser     for Racket/Scheme
@item eval-in-repl-racket     for Racket
@item eval-in-repl-scheme     for Scheme
@item eval-in-repl-hy         for Hy
@item eval-in-repl-python     for Python
@item eval-in-repl-ruby       for Ruby
@item eval-in-repl-sml        for Standard ML
@item eval-in-repl-ocaml      for OCaml
@item eval-in-repl-prolog     for Prolog
@item eval-in-repl-javascript for Javascript
@item eval-in-repl-shell      for Shell
@item eval-in-repl-iex        for Elixir
@item eval-in-repl-erlang     for Erlang
@item eval-in-repl-elm        for Elm
@end itemize")
    (license license:expat)))

(define-public emacs-ob-elm
  (let ((commit "d3a9fbc2f56416894c9aed65ea9a20cc1d98f15d")
        (revision "0"))
    (package
      (name "emacs-ob-elm")
      (version (git-version "0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/BonfaceKilz/ob-elm")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1wdlr0cbsb2drdmcn2bnivjkj1f2v52l6yizwsnjgi4xq3w6k56h"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/BonfaceKilz/ob-elm")
      (synopsis "Org-Babel support for evaluating Elm code")
      (description
"This package adds Org-Babel support for evaluating Elm code, which can be
enabled by adding @code{(elm . t)} to the variable
@code{org-babel-do-load-languages}.")
      (license license:gpl3+))))

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
    (synopsis "Eval org-mode babel code blocks in various REPLs")
    (description
     "This package allows you to execute org-mode (babel) source code blocks with
eval-in-repl.  It features async execution (because it uses an external
process) and babel execution without the output written in the buffer (less
visual distraction, and the output is reproducible as long as the code is
saved).")
    (license license:expat)))

(define-public emacs-magit-stgit
  (let* ((commit "8b31b21fca4c528f90ac79960c405d738b62c889")
         (revision "0"))
    (package
      (name "emacs-magit-stgit")
      (version (git-version "2.1.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/nicolas-graves/magit-stgit")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "14ng6kmrrklbdas891wsjnf4xa9rki12anhsjgip4byv9d5b89hm"))))
      (build-system emacs-build-system)
      (inputs (list emacs-dash emacs-magit emacs-magit-popup))
      (propagated-inputs (list stgit))
      (home-page "https://github.com/nicolas-graves/magit-stgit")
      (synopsis "magit-stgit")
      (description "magit-stgit")
      (license license:gpl3+))))

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

(define-public emacs-orca
  (let* ((commit "0687f416a5573f63b691d384454f5a793266ed97")
         (revision "1"))
    (package
      (name "emacs-orca")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/nicolas-graves/orca")
               (commit commit)))
         (sha256
          (base32
           "00a363vkqvryw5s7pj0kh8pqq5vvbf1pmbzz0b1z1fckwr49sv0f"))))
      (build-system emacs-build-system)
      (propagated-inputs (list emacs-zoutline))
      (home-page "https://github.com/abo-abo/orca")
      (synopsis "Handler for Org Capture")
      (description
       "This package provides several convenient recipes for configuring
@code{org-capture}, mainly for capturing from a browser. It can match urls and
inject the capture in a targeted org file, under a targeted heading. The more
this package is configured, the less refiling is needed on your captures: they
will go directly to where they belong.")
      (license license:gpl3+))))

(define-public emacs-app-launcher
  (let* ((commit "d5015e394b0a666a8c7c4d4bdf786266e773b145")
         (revision "0"))
    (package
    (name "emacs-app-launcher")
    (version (git-version "0" revision commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/SebastienWae/app-launcher")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l97ajy27awydyd4gc6323wyhpm5vm2db6i0lp5gqaxi9fp7jivp"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/SebastienWae/app-launcher")
    (synopsis "Use Emacs standard completion to launch applications")
    (description "This package defines the app-launcher-run-app command which
uses Emacs standard completion to select an application installed on your
machine and launch it.")
    (license license:gpl3))))

(define (emacs-eval-in-repl-langage name lang hash inputs)
  (define pack
    (package
      (name (string-append "emacs-eval-in-repl-" (symbol->string name)))
      (version "0.9.7")
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://raw.githubusercontent.com/kaz-yos/eval-in-repl/"
               version "/eval-in-repl-" name ".el"))
         (sha256
          (base32 hash))))
      (build-system emacs-build-system)
      (propagated-inputs (append (list emacs-eval-in-repl) inputs))
      (home-page "https://github.com/kaz-yos/eval-in-repl")
      (synopsis "Consistent evaluation interface for Emacs Lisp REPLs for emacs")
      (description (string-append "\
This package provides a consistent ESS-like evaluation interface for
" lang " REPLs.  Emacs Speaks Statistics (ESS) package has a nice function
called @code{ess-eval-region-or-line-and-step}, which is assigned to
C-RET.  This function sends a line or a selected region to the corresponding
shell (R, Julia, Stata, etc) visibly.  It also start up a shell if there is
none."))
      (license license:expat)))
  pack)

(define-public emacs-eval-in-repl-ielm
  (emacs-eval-in-repl-langage
   'ielm
   "Emacs Lisp"
   "1inm0siq0ybgcrdi1pqzawqqvx1f2540yk3s8r5cd2m6fnrjwllv"
   '()))

(define-public emacs-eval-in-repl-cider
  (emacs-eval-in-repl-langage
   'cider
   "Clojure"
   "047sv99iv2zimv26wncnq7r8x1gjncfcmrxnprgx6s4vm5y217qj"
   (list emacs-cider)))

(define-public emacs-eval-in-repl-slime
  (emacs-eval-in-repl-langage
   'slime
   "Common Lisp"
   "0qj4dkkkf1xgvcy6wz537w5d2aqnwc75w8g9qzdsfyadaiycgrsd"
   (list emacs-slime)))

(define-public emacs-eval-in-repl-geiser
  (emacs-eval-in-repl-langage
   'geiser
   "Racket/Scheme"
   "0x2v51hwm1iaa0r8mn34i08vck5y32njfwfiq0c0blbfmjsqlyz2"
   (list emacs-geiser)))

(define-public emacs-eval-in-repl-racket
  (emacs-eval-in-repl-langage
   'racket
   "Racket"
   "0wpkig2z2vfyv08i444fi2yhjy2mk0la8mpyg0z6zywjm19kyir3"
   (list emacs-racket-mode)))

(define-public emacs-eval-in-repl-scheme
  (emacs-eval-in-repl-langage
   'scheme
   "Scheme"
   "0qc2gipr2pm80d3jjxzwbca4wbl0jhb5mp6gfz0qkagffwiv9dpi"
   '()))

(define-public emacs-eval-in-repl-hy
  (emacs-eval-in-repl-langage
   'hy
   "Hy"
   "1fcf2a6vrmwvd2blh97mfdrzmym2g6q0b63s51p1k5gw7ijz0i4r"
   (list emacs-hy-mode)))

(define-public emacs-eval-in-repl-python
  (emacs-eval-in-repl-langage
   'python
   "Python"
   "06abhykzz41wz8h3gr0x0ljiva9rfgpagija24afpdg8l2w0b3jn"
   '()))

(define-public emacs-eval-in-repl-ruby
  (emacs-eval-in-repl-langage
   'ruby
   "Ruby"
   "05yrv9pj91yfxk46g5ky9xixndgmzv0c4nhn4qsn85mx3jy9x915"
   (list emacs-inf-ruby)))

(define-public emacs-eval-in-repl-sml
  (emacs-eval-in-repl-langage
   'sml
   "Standard ML"
   "0g36fmc5khdkcyax7rnxmnvir43mig9s4mlgr8fkcffxvb2asw7d"
   (list emacs-sml-mode)))

(define-public emacs-eval-in-repl-ocaml
  (emacs-eval-in-repl-langage
   'ocaml
   "OCaml"
   "0y36x59adjf87ypfj62rrhdf6lg8qxyahvx9f7p1svblhryg7fr0"
   (list emacs-tuareg)))

(define-public emacs-eval-in-repl-prolog
  (emacs-eval-in-repl-langage
   'prolog
   "Prolog"
   "0plbi5jrcpzd8jphrsha3ng707qhdysng8xf1ypg4qi0xg9qkh0c"
   '()))

(define-public emacs-eval-in-repl-javascript
  (emacs-eval-in-repl-langage
   'javascript
   "Prolog"
   "0plbi5jrcpzd8jphrsha3ng707qhdysng8xf1ypg4qi0xg9qkh0c" ; FIXME
   (list emacs-js2-mode emacs-js-comint)))

(define-public emacs-eval-in-repl-shell
  (emacs-eval-in-repl-langage
   'shell
   "Shell"
   "0plbi5jrcpzd8jphrsha3ng707qhdysng8xf1ypg4qi0xg9qkh0c"
   '()))
 ; FIXME

(define-public emacs-eval-in-repl-iex
  (emacs-eval-in-repl-langage
   'iex
   "Elixir"
   "0ygwf9d739zqc8dcckw0j0bqkipw7cmxbrx3l281x237a3d384yw"
   (list emacs-elixir-mode emacs-alchemist)))

(define-public emacs-eval-in-repl-erlang
  (emacs-eval-in-repl-langage
   'erlang
   "Erlang"
   "1gk0kgi5j22lszjrna4l79pq8zqyq6g35pk5issacw9jx179nb7n"
   (list emacs-erlang)))

(define-public emacs-eval-in-repl-elm
  (emacs-eval-in-repl-langage
   'elm
   "Elm"
   "0ca6070y7s86xs4y1dibq6b1rz143z5i17s7ifra0afgib10a5hb"
   (list emacs-elm-mode)))
