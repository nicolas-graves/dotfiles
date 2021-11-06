(define-module (home services mu4e)
  #:use-module (gnu home services)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages emacs)
  #:use-module (gnu services configuration)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 pretty-print)

  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (home-mu4e-service-type
	    home-mu4e-configuration
	    home-mu4e-extension))

(define packages? (list-of package?))

(define serialize-packages empty-serializer)
(define serialize-boolean empty-serializer)

(define elisp-config? list?)
(define (serialize-elisp-config field-name val)
  (define (serialize-list-element elem)
    (cond
     ((gexp? elem)
      elem)
     (else
      #~(string-trim-right
	   (with-output-to-string
	     (lambda ()
	       ((@@ (ice-9 pretty-print) pretty-print)
		'#$elem
                #:max-expr-width 79)))
	   #\newline))))

  #~(string-append
     #$@(interpose
	 (map serialize-list-element val)
	 "\n" 'suffix)))

(define-configuration home-mu4e-configuration
  (package
   (package emacs)
   "Mu4e package to use.")
  (xdg-flavor?
   (boolean #t)
   "Whether to place all the configuration files in
@file{$XDG_CONFIG_HOME/emacs}.")
  (config
   (elisp-config '())
   "List of expressions, each expression can be a Sexp or Gexp.

Sexp is a Emacs Lisp form, preferably valid.  Be aware, if you include
values of Guile variables, they won't be automatically converted to
Elisp.  Strings doesn't require conversion, but for example booleans
do: @code{#t} -> @code{t}, @code{#f} -> @code{nil}.  Be careful here.

However, Sexp can contain file-like objects; String with path to a
corresponding file will appear in place of each such object.  See an
example below for more details.

Gexp should be string-valued.  The value of Gexp will be appended to
resulting Emacs Lisp file.

The list of expressions will be interposed with \\n and everything
will end up in @file{init.el}.

@example
(let ((guile-bool-value #f))
  (home-emacs-configuration
   (init-el
    `((setq rg-binary ,(file-append ripgrep \"/bin/rg\"))
      (load-file ,(local-file \"./emacs/test-init.el\"))
      \"just a string\"
      ;; Make sure you converted guile values to Elisp
      (setq tmp-boolean ,(if guile-bool-value 't 'nil))
      ,(if guile-bool-value '(setq v1 nil) '(setq v2 t))

      ,#~\"\\n;;; Section with gexps results:\"

      ,(slurp-file-gexp (local-file \"./emacs/test-init.el\"))
      ,#~(string-append \"(princ \" \"'hello)\")
      ,#~\"\\n\"
      ,#~\";; Another comment\"))))
@end example

would yield something like:

@example
(setq rg-binary
      \"/gnu/store/dw884p9d2jb83j4fqvdj2i10fn9xgwqd-ripgrep-12.1.1/bin/rg\")
(load-file
  \"/gnu/store/9b1s48crng5dy9xmxskcdnillw18bkg2-test-init.el\")
\"just a string\"
(setq tmp-boolean nil)
(setq v2 t)

;;; Section with gexps results:
;; Here is
\"a sample\"
;; content of test-init.el

(princ 'hello)


;; Another comment
@end example"))

(define (add-mu4e-configuration config)
  (let* ((xdg-flavor? (home-mu4e-configuration-xdg-flavor? config)))
    (define prefix-file
      (cut string-append
	(if xdg-flavor?
	    "config/emacs/"
	    "emacs.d/")
	<>))

    (define (filter-fields field)
      (filter-configuration-fields home-mu4e-configuration-fields
				   (list field)))

    (define (serialize-field field)
      (serialize-configuration
       config
       (filter-fields field)))

    (filter
     (compose not null?)
     (list
      (let ((file-name "mail.el")
            (field-obj (car (filter-fields 'config))))
        (optional (not (null? ((configuration-field-getter field-obj) config)))
                  `(,(prefix-file file-name)
                    ,(mixed-text-file
                      file-name
                      (serialize-field 'config)))))
      ))))

(define home-mu4e-service-type
  (service-type (name 'home-mu4e)
                (extensions
                 (list (service-extension
                        home-files-service-type
                        add-mu4e-configuration)))
		(compose identity)
                (default-value (home-mu4e-configuration))
                (description "Install and configure GNU Emacs, the
extensible, self-documenting editor.")))

(define (generate-home-mu4e-documentation)
  (generate-documentation
   `((home-mu4e-configuration
      ,home-mu4e-configuration-fields))
   'home-mu4e-configuration))
