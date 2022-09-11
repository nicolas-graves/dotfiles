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
    (synopsis "Consistent ESS-like eval interface for various REPLs for emacs")
    (description
     "This package provides a consistent ESS-like eval
interface for various REPLs.
Emacs Speaks Statistics (ESS) package has a nice function called
ess-eval-region-or-line-and-step, which is assigned to C-RET. This function
sends a line or a selected region to the corresponding shell (R, Julia, Stata,
etc) visibly. It also start up a shell if there is none.

This package along with REPL/shell specific packages implement similar work flow
for various REPLs.

This package alone is not functional. It also requires the following packages
depending on your needs :
@itemize
@item eval-in-repl-ielm    for Emacs Lisp
@item eval-in-repl-cider   for Clojure
@item eval-in-repl-slime   for Common Lisp
@item eval-in-repl-geiser  for Racket/Scheme
@item eval-in-repl-racket  for Racket
@item eval-in-repl-scheme  for Scheme
@item eval-in-repl-hy      for Hy
@item eval-in-repl-python  for Python
@item eval-in-repl-ruby    for Ruby
@item eval-in-repl-sml     for Standard ML
@item eval-in-repl-ocaml   for OCaml
@item eval-in-repl-prolog  for Prolog
@item eval-in-repl-javascript for Javascript
@item eval-in-repl-shell   for Shell
@item eval-in-repl-iex     for Elixir
@item eval-in-repl-erlang  for Erlang
@item eval-in-repl-elm     for Elm
@end itemize")
    (license license:expat)))

(define-public emacs-eval-in-repl-ielm
  (package
    (name "emacs-eval-in-repl-ielm")
    (version "0.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://raw.githubusercontent.com/kaz-yos/eval-in-repl/"
               version "/eval-in-repl-ielm.el"))
       (sha256
          (base32
           "1inm0siq0ybgcrdi1pqzawqqvx1f2540yk3s8r5cd2m6fnrjwllv"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-eval-in-repl))
    (home-page "https://github.com/kaz-yos/eval-in-repl")
    (synopsis "Consistent ESS-like eval interface for Emacs Lisp REPLs for emacs")
    (description
     "This package provides a consistent ESS-like eval interface for Emacs Lisp
REPLs. Emacs Speaks Statistics (ESS) package has a nice function called
ess-eval-region-or-line-and-step, which is assigned to C-RET. This function
sends a line or a selected region to the corresponding shell (R, Julia, Stata,
etc) visibly. It also start up a shell if there is none. This package implement
similar work flow for Emacs Lisp REPLs.")
    (license license:expat)))

(define-public emacs-eval-in-repl-cider
  (package
    (name "emacs-eval-in-repl-cider")
    (version "0.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://raw.githubusercontent.com/kaz-yos/eval-in-repl/"
               version "/eval-in-repl-cider.el"))
       (sha256
          (base32
           "047sv99iv2zimv26wncnq7r8x1gjncfcmrxnprgx6s4vm5y217qj"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-eval-in-repl emacs-cider))
    (home-page "https://github.com/kaz-yos/eval-in-repl")
    (synopsis "Consistent ESS-like eval interface for Clojure REPLs for emacs")
    (description
     "This package provides a consistent ESS-like eval interface for Clojure
REPLs. Emacs Speaks Statistics (ESS) package has a nice function called
ess-eval-region-or-line-and-step, which is assigned to C-RET. This function
sends a line or a selected region to the corresponding shell (R, Julia, Stata,
etc) visibly. It also start up a shell if there is none. This package
implement similar work flow for Clojure REPLs.")
    (license license:expat)))

(define-public emacs-eval-in-repl-slime
  (package
    (name "emacs-eval-in-repl-slime")
    (version "0.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://raw.githubusercontent.com/kaz-yos/eval-in-repl/"
               version "/eval-in-repl-slime.el"))
       (sha256
          (base32
           "0qj4dkkkf1xgvcy6wz537w5d2aqnwc75w8g9qzdsfyadaiycgrsd"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-eval-in-repl emacs-slime))
    (home-page "https://github.com/kaz-yos/eval-in-repl")
    (synopsis "Consistent ESS-like eval interface for Common Lisp REPLs for emacs")
    (description
     "This package provides a consistent ESS-like eval interface for Common Lisp
REPLs. Emacs Speaks Statistics (ESS) package has a nice function called
ess-eval-region-or-line-and-step, which is assigned to C-RET. This function
sends a line or a selected region to the corresponding shell (R, Julia, Stata,
etc) visibly. It also start up a shell if there is none. This package
implement similar work flow for Common Lisp REPLs.")
    (license license:expat)))

(define-public emacs-eval-in-repl-geiser
  (package
    (name "emacs-eval-in-repl-geiser")
    (version "0.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://raw.githubusercontent.com/kaz-yos/eval-in-repl/"
             version "/eval-in-repl-geiser.el"))
       (sha256
        (base32
         "0x2v51hwm1iaa0r8mn34i08vck5y32njfwfiq0c0blbfmjsqlyz2"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-eval-in-repl emacs-geiser))
    (home-page "https://github.com/kaz-yos/eval-in-repl")
    (synopsis "Consistent ESS-like eval interface for Racket/Scheme REPLs for emacs")
    (description
     "This package provides a consistent ESS-like eval interface for
Racket/Scheme REPLs. Emacs Speaks Statistics (ESS) package has a nice function
called ess-eval-region-or-line-and-step, which is assigned to C-RET. This
function sends a line or a selected region to the corresponding shell (R, Julia,
Stata, etc) visibly. It also start up a shell if there is none. This package
implement similar work flow for Racket/Scheme REPLs.")
    (license license:expat)))

(define-public emacs-eval-in-repl-racket
  (package
    (name "emacs-eval-in-repl-racket")
    (version "0.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://raw.githubusercontent.com/kaz-yos/eval-in-repl/"
               version "/eval-in-repl-racket.el"))
       (sha256
          (base32
           "0wpkig2z2vfyv08i444fi2yhjy2mk0la8mpyg0z6zywjm19kyir3"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-eval-in-repl emacs-racket-mode))
    (home-page "https://github.com/kaz-yos/eval-in-repl")
    (synopsis "Consistent ESS-like eval interface for Racket REPLs for emacs")
    (description
     "This package provides a consistent ESS-like eval interface for Racket
REPLs. Emacs Speaks Statistics (ESS) package has a nice function called
ess-eval-region-or-line-and-step, which is assigned to C-RET. This function
sends a line or a selected region to the corresponding shell (R, Julia, Stata,
etc) visibly. It also start up a shell if there is none. This package implement
similar work flow for Racket REPLs.")
    (license license:expat)))

(define-public emacs-eval-in-repl-scheme
  (package
    (name "emacs-eval-in-repl-scheme")
    (version "0.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://raw.githubusercontent.com/kaz-yos/eval-in-repl/"
               version "/eval-in-repl-scheme.el"))
       (sha256
          (base32
           "0qc2gipr2pm80d3jjxzwbca4wbl0jhb5mp6gfz0qkagffwiv9dpi"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-eval-in-repl))
    (home-page "https://github.com/kaz-yos/eval-in-repl")
    (synopsis "Consistent ESS-like eval interface for Scheme REPLs for emacs")
    (description
     "This package provides a consistent ESS-like eval interface for Scheme
REPLs. Emacs Speaks Statistics (ESS) package has a nice function called
ess-eval-region-or-line-and-step, which is assigned to C-RET. This function
sends a line or a selected region to the corresponding shell (R, Julia, Stata,
etc) visibly. It also start up a shell if there is none. This package implement
similar work flow for Scheme REPLs.")
    (license license:expat)))

(define-public emacs-eval-in-repl-hy
  (package
    (name "emacs-eval-in-repl-hy")
    (version "0.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://raw.githubusercontent.com/kaz-yos/eval-in-repl/"
               version "/eval-in-repl-hy.el"))
       (sha256
          (base32
           "1fcf2a6vrmwvd2blh97mfdrzmym2g6q0b63s51p1k5gw7ijz0i4r"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-eval-in-repl emacs-hy-mode))
    (home-page "https://github.com/kaz-yos/eval-in-repl")
    (synopsis "Consistent ESS-like eval interface for Hy REPLs for emacs")
    (description
     "This package provides a consistent ESS-like eval interface for Hy REPLs.
Emacs Speaks Statistics (ESS) package has a nice function called
ess-eval-region-or-line-and-step, which is assigned to C-RET. This function
sends a line or a selected region to the corresponding shell (R, Julia, Stata,
etc) visibly. It also start up a shell if there is none. This package
implement similar work flow for Hy REPLs.")
    (license license:expat)))

(define-public emacs-eval-in-repl-python
  (package
    (name "emacs-eval-in-repl-python")
    (version "0.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://raw.githubusercontent.com/kaz-yos/eval-in-repl/"
             version "/eval-in-repl-python.el"))
       (sha256
        (base32
         "06abhykzz41wz8h3gr0x0ljiva9rfgpagija24afpdg8l2w0b3jn"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-eval-in-repl))
    (home-page "https://github.com/kaz-yos/eval-in-repl")
    (synopsis "Consistent ESS-like eval interface for Python REPLs for emacs")
    (description
     "This package provides a consistent ESS-like eval interface for Python
REPLs. Emacs Speaks Statistics (ESS) package has a nice function called
ess-eval-region-or-line-and-step, which is assigned to C-RET. This function
sends a line or a selected region to the corresponding shell (R, Julia, Stata,
etc) visibly. It also start up a shell if there is none. This package
implement similar work flow for Python REPLs.")
    (license license:expat)))

(define-public emacs-eval-in-repl-ruby
  (package
    (name "emacs-eval-in-repl-ruby")
    (version "0.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://raw.githubusercontent.com/kaz-yos/eval-in-repl/"
             version "/eval-in-repl-ruby.el"))
       (sha256
        (base32
         "05yrv9pj91yfxk46g5ky9xixndgmzv0c4nhn4qsn85mx3jy9x915"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-eval-in-repl emacs-inf-ruby))
    (home-page "https://github.com/kaz-yos/eval-in-repl")
    (synopsis "Consistent ESS-like eval interface for Ruby REPLs for emacs")
    (description
     "This package provides a consistent ESS-like eval interface for Ruby REPLs.
Emacs Speaks Statistics (ESS) package has a nice function called
ess-eval-region-or-line-and-step, which is assigned to C-RET. This function
sends a line or a selected region to the corresponding shell (R, Julia, Stata,
etc) visibly. It also start up a shell if there is none. This package
implement similar work flow for Ruby REPLs.")
    (license license:expat)))

(define-public emacs-eval-in-repl-sml
  (package
    (name "emacs-eval-in-repl-sml")
    (version "0.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://raw.githubusercontent.com/kaz-yos/eval-in-repl/"
             version "/eval-in-repl-sml.el"))
       (sha256
        (base32
         "0g36fmc5khdkcyax7rnxmnvir43mig9s4mlgr8fkcffxvb2asw7d"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-eval-in-repl emacs-sml-mode))
    (home-page "https://github.com/kaz-yos/eval-in-repl")
    (synopsis "Consistent ESS-like eval interface for Standard ML REPLs for emacs")
    (description
     "This package provides a consistent ESS-like eval interface for Standard ML
REPLs. Emacs Speaks Statistics (ESS) package has a nice function called
ess-eval-region-or-line-and-step, which is assigned to C-RET. This function
sends a line or a selected region to the corresponding shell (R, Julia, Stata,
etc) visibly. It also start up a shell if there is none. This package
implement similar work flow for Standard ML REPLs.")
    (license license:expat)))

(define-public emacs-eval-in-repl-ocaml
  (package
    (name "emacs-eval-in-repl-ocaml")
    (version "0.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://raw.githubusercontent.com/kaz-yos/eval-in-repl/"
             version "/eval-in-repl-ocaml.el"))
       (sha256
        (base32
         "0y36x59adjf87ypfj62rrhdf6lg8qxyahvx9f7p1svblhryg7fr0"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-eval-in-repl emacs-tuareg))
    (home-page "https://github.com/kaz-yos/eval-in-repl")
    (synopsis "Consistent ESS-like eval interface for OCaml REPLs for emacs")
    (description
     "This package provides a consistent ESS-like eval interface for OCaml
REPLs. Emacs Speaks Statistics (ESS) package has a nice function called
ess-eval-region-or-line-and-step, which is assigned to C-RET. This function
sends a line or a selected region to the corresponding shell (R, Julia, Stata,
etc) visibly. It also start up a shell if there is none. This package
implement similar work flow for OCaml REPLs.")
    (license license:expat)))

(define-public emacs-eval-in-repl-prolog
  (package
    (name "emacs-eval-in-repl-prolog")
    (version "0.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://raw.githubusercontent.com/kaz-yos/eval-in-repl/"
             version "/eval-in-repl-prolog.el"))
       (sha256
        (base32
         "0plbi5jrcpzd8jphrsha3ng707qhdysng8xf1ypg4qi0xg9qkh0c"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-eval-in-repl))
    (home-page "https://github.com/kaz-yos/eval-in-repl")
    (synopsis "Consistent ESS-like eval interface for Prolog REPLs for emacs")
    (description
     "This package provides a consistent ESS-like eval interface for Prolog
REPLs. Emacs Speaks Statistics (ESS) package has a nice function called
ess-eval-region-or-line-and-step, which is assigned to C-RET. This function
sends a line or a selected region to the corresponding shell (R, Julia, Stata,
etc) visibly. It also start up a shell if there is none. This package
implement similar work flow for Prolog REPLs.")
    (license license:expat)))

(define-public emacs-eval-in-repl-javascript
  (package
    (name "emacs-eval-in-repl-javascript")
    (version "0.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://raw.githubusercontent.com/kaz-yos/eval-in-repl/"
             version "/eval-in-repl-javascript.el"))
       (sha256
        (base32
         "09gfd184waa3w4wlz36ys3rj79ms0584j6jibrqww6313h81ny2x"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-eval-in-repl
                             emacs-js2-mode
                             emacs-js-comint))
    (home-page "https://github.com/kaz-yos/eval-in-repl")
    (synopsis "Consistent ESS-like eval interface for Javascript REPLs for emacs")
    (description
     "This package provides a consistent ESS-like eval interface for Javascript
REPLs. Emacs Speaks Statistics (ESS) package has a nice function called
ess-eval-region-or-line-and-step, which is assigned to C-RET. This function
sends a line or a selected region to the corresponding shell (R, Julia, Stata,
etc) visibly. It also start up a shell if there is none. This package
implement similar work flow for Javascript REPLs.")
    (license license:expat)))

(define-public emacs-eval-in-repl-shell
  (package
    (name "emacs-eval-in-repl-shell")
    (version "0.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://raw.githubusercontent.com/kaz-yos/eval-in-repl/"
             version "/eval-in-repl-shell.el"))
       (sha256
        (base32
         "1jsi8wjibx4v4ysb2xf96g03vqg7n41sxyg5bp8w82qlfjszdnix"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-eval-in-repl))
    (home-page "https://github.com/kaz-yos/eval-in-repl")
    (synopsis "Consistent ESS-like eval interface for Shell REPLs for emacs")
    (description
     "This package provides a consistent ESS-like eval interface for Shell
REPLs. Emacs Speaks Statistics (ESS) package has a nice function called
ess-eval-region-or-line-and-step, which is assigned to C-RET. This function
sends a line or a selected region to the corresponding shell (R, Julia, Stata,
etc) visibly. It also start up a shell if there is none. This package
implement similar work flow for Shell REPLs.")
    (license license:expat)))

(define-public emacs-eval-in-repl-iex
  (package
    (name "emacs-eval-in-repl-iex")
    (version "0.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://raw.githubusercontent.com/kaz-yos/eval-in-repl/"
               version "/eval-in-repl-iex.el"))
       (sha256
          (base32
           "1qj943bv7vx6rhahkwl619zwjal7agq6ry1cdqwlcvq0mz1ds00r"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-eval-in-repl
                             emacs-elixir-mode
                             emacs-alchemist))
    (home-page "https://github.com/kaz-yos/eval-in-repl")
    (synopsis "Consistent ESS-like eval interface for Elixir REPLs for emacs")
    (description
     "This package provides a consistent ESS-like eval interface for Elixir
REPLs. Emacs Speaks Statistics (ESS) package has a nice function called
ess-eval-region-or-line-and-step, which is assigned to C-RET. This function
sends a line or a selected region to the corresponding shell (R, Julia, Stata,
etc) visibly. It also start up a shell if there is none. This package
implement similar work flow for Elixir REPLs.")
    (license license:expat)))

(define-public emacs-eval-in-repl-erlang
  (package
    (name "emacs-eval-in-repl-erlang")
    (version "0.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://raw.githubusercontent.com/kaz-yos/eval-in-repl/"
               version "/eval-in-repl-erlang.el"))
       (sha256
          (base32
           "1gk0kgi5j22lszjrna4l79pq8zqyq6g35pk5issacw9jx179nb7n"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-eval-in-repl emacs-erlang))
    (home-page "https://github.com/kaz-yos/eval-in-repl")
    (synopsis "Consistent ESS-like eval interface for Erlang REPLs for emacs")
    (description
     "This package provides a consistent ESS-like eval interface for Erlang
REPLs. Emacs Speaks Statistics (ESS) package has a nice function called
ess-eval-region-or-line-and-step, which is assigned to C-RET. This function
sends a line or a selected region to the corresponding shell (R, Julia, Stata,
etc) visibly. It also start up a shell if there is none. This package
implement similar work flow for Erlang REPLs.")
    (license license:expat)))

(define-public emacs-eval-in-repl-elm
  (package
    (name "emacs-eval-in-repl-elm")
    (version "0.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://raw.githubusercontent.com/kaz-yos/eval-in-repl/"
               version "/eval-in-repl-elm.el"))
       (sha256
          (base32
           "0ca6070y7s86xs4y1dibq6b1rz143z5i17s7ifra0afgib10a5hb"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-eval-in-repl emacs-elm-mode))
    (home-page "https://github.com/kaz-yos/eval-in-repl")
    (synopsis "Consistent ESS-like eval interface for Elm REPLs for emacs")
    (description
     "This package provides a consistent ESS-like eval interface for Elm REPLs.
Emacs Speaks Statistics (ESS) package has a nice function called
ess-eval-region-or-line-and-step, which is assigned to C-RET. This function
sends a line or a selected region to the corresponding shell (R, Julia, Stata,
etc) visibly. It also start up a shell if there is none. This package
implement similar work flow for Elm REPLs.")
    (license license:expat)))

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
"@code{emacs-ob-elm} adds Org-Babel support for evaluating Elm code.")
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
    (synopsis "Eval org-mode babel code blocks in various REPLs.")
    (description
     "This package allows you to execute org-mode (babel) source code blocks with
eval-in-repl. It features async execution (because it uses an external
process!) and babel execution without the output written in the buffer (Less
visual distraction! Output is reproducible as long as the code is saved). ")
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