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
        (base32 "0ygwf9d739zqc8dcckw0j0bqkipw7cmxbrx3l281x237a3d384yw"))))
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

(define (repl->lang s)
    (cond
     ((string= s "ielm") "Emacs Lisp")
     ((string= s "cider") "Clojure")
     ((string= s "slime") "Common Lisp")
     ((string= s "geiser") "Racket/Scheme")
     ((string= s "sml") "Standard ML")
     ((string= s "iex") "Elixir")
     ((string= s "ocaml") "OCaml")
     (else (string-capitalize s))))

(define (lang->inputs s)
    (cond
     ((string= s "Clojure") (list emacs-cider))
     ((string= s "Common Lisp") (list emacs-slime))
     ((string= s "Racket/Scheme") (list emacs-geiser))
     ((string= s "Racket") (list emacs-racket-mode))
     ((string= s "Hy") (list emacs-hy-mode))
     ((string= s "Ruby") (list emacs-inf-ruby))
     ((string= s "Standard ML") (list emacs-sml-mode))
     ((string= s "OCaml") (list emacs-tuareg))
     ((string= s "Javascript") (list emacs-js2-mode emacs-js-comint))
     ((string= s "Elixir") (list emacs-elixir-mode emacs-alchemist))
     ((string= s "Erlang") (list emacs-erlang))
     ((string= s "Elm") (list emacs-elm-mode))
     (else '())))

(define (pack-eval-in-repl repl)
  (let* ((lang (repl->lang repl))
         (inputs (lang->inputs lang)))
    (package
      (name (string-append "emacs-eval-in-repl-" repl))
      (version "0.9.7")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kaz-yos/eval-in-repl")
               (commit (string-append "v" version))))

         (file-name (git-file-name name version))
         (sha256
          (base32 "1mrssbl0wyc6iij8zk1y3h9bd3rv53nnrxsij7fn67l1m4z0clyn"))))
      (build-system emacs-build-system)
      (arguments
       `(#:include (list (string-append "eval-in-repl-" ,repl "\\.el"))))
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
      (license license:expat))))

(define-public emacs-eval-in-repl-ielm       (pack-eval-in-repl "ielm"))
(define-public emacs-eval-in-repl-cider      (pack-eval-in-repl "cider"))
(define-public emacs-eval-in-repl-slime      (pack-eval-in-repl "slime"))
(define-public emacs-eval-in-repl-geiser     (pack-eval-in-repl "geiser"))
(define-public emacs-eval-in-repl-racket     (pack-eval-in-repl "racket"))
(define-public emacs-eval-in-repl-scheme     (pack-eval-in-repl "scheme"))
(define-public emacs-eval-in-repl-hy         (pack-eval-in-repl "hy"))
(define-public emacs-eval-in-repl-python     (pack-eval-in-repl "python"))
(define-public emacs-eval-in-repl-ruby       (pack-eval-in-repl "ruby"))
(define-public emacs-eval-in-repl-sml        (pack-eval-in-repl "sml"))
(define-public emacs-eval-in-repl-ocaml      (pack-eval-in-repl "ocaml"))
(define-public emacs-eval-in-repl-prolog     (pack-eval-in-repl "prolog"))
(define-public emacs-eval-in-repl-javascript (pack-eval-in-repl "javascript"))
(define-public emacs-eval-in-repl-shell      (pack-eval-in-repl "shell"))
(define-public emacs-eval-in-repl-iex        (pack-eval-in-repl "iex"))
(define-public emacs-eval-in-repl-erlang     (pack-eval-in-repl "erlang"))
(define-public emacs-eval-in-repl-elm        (pack-eval-in-repl "elm"))
