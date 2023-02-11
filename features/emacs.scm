;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2022 Samuel Culpepper <samuel@samuelculpepper.com>
;;; Copyright © 2022 Demis Balbach <db@minikn.xyz>
;;; Copyright © 2021, 2022 Nicolas Graves <ngraves@ngraves.fr>
;;;
;;; This file is part of rde.
;;;
;;; rde is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; rde is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with rde.  If not, see <http://www.gnu.org/licenses/>.

(define-module (features emacs)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (rde home services emacs-xyz)
  #:use-module (gnu home services)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home-services wm)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu services)

  #:use-module (rde packages)
  #:use-module (rde packages emacs)
  #:use-module (rde packages emacs-xyz)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages package-management)

  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (guix transformations)

  #:export (feature-emacs-flycheck
            feature-emacs-web-mode
            feature-emacs-guix-development
            feature-emacs-org-babel
            feature-emacs-python
            feature-emacs-eval-in-repl))

(define* (feature-emacs-web-mode
          #:key
          (emacs-web-mode emacs-web-mode)
          (rainbow-mode? #f))
  "Configure web-mode for emacs."
  (ensure-pred boolean? rainbow-mode?)

  (define emacs-f-name 'web-mode)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile (require 'web-mode))
        (push '("(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'" . web-mode) auto-mode-alist)
        (setq-default web-mode-code-indent-offset 2)
        (setq-default web-mode-markup-indent-offset 2)
        (setq-default web-mode-attribute-indent-offset 2)

        ,@(if rainbow-mode?
              `((eval-when-compile (require 'rainbow-mode))
                (add-hook 'org-mode-hook 'rainbow-mode)
                (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
                (add-hook 'web-mode-hook 'rainbow-mode)
                (add-hook 'js2-mode-hook 'rainbow-mode))
              '()))
      #:elisp-packages
      (append (if rainbow-mode? (list emacs-rainbow-mode) '())
              (list emacs-web-mode))
      #:summary "\
WEB-MODE"
      #:commentary "\
")))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-web-mode)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-python
          #:key
          (emacs-python-black emacs-python-black)
          (black? #f))
  "Configure python for emacs."

  (define emacs-f-name 'python)
  (define f-name (symbol-append 'emacs- emacs-f-name))
  (ensure-pred boolean? black?)

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile (require 'python-black))
        (add-hook 'python-mode 'python-black-on-save-mode-enable-dwim))
      #:elisp-packages
      (list emacs-python-black)
      #:summary "\
Python"
      #:commentary "\
")))

  (feature
   (name f-name)
   (values `((,f-name . 'emacs-python)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-flycheck
          #:key
          (emacs-flycheck emacs-flycheck))
  "Configure flycheck for emacs."

  (define emacs-f-name 'flycheck)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile (require 'flycheck))
        (add-hook 'eglot-managed-mode-hook
                  'flycheck-mode))
      #:elisp-packages (list emacs-flycheck)
      #:summary "\
FLYCHECK"
      #:commentary "\
")))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-flycheck)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-org-babel
          #:key
          (load-language-list (list "emacs-lisp"))
          (block-templates? #f))

  "Configure org-babel for emacs."
  (ensure-pred list? load-language-list)
  (ensure-pred boolean? block-templates?)

  (define emacs-f-name 'org-babel)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile (require 'ob-tangle))
        (with-eval-after-load
         'org-babel
         (setq org-edit-src-content-indentation 0
               org-src-tab-acts-natively t
               org-src-preserve-indentation t)
         (org-babel-do-load-languages
          'org-babel-load-languages
          '(,@(cons* (map
                      (lambda (babel-lang)
                        `(,(string->symbol babel-lang) . t))
                      load-language-list))))
         ,@(if (member "dot" load-language-list)
               `((setq org-src-lang-modes
                       (delete '("dot" . fundamental) org-src-lang-modes))
                 (push '(("conf-unix" . conf-unix)
                         ("dot" . graphviz-dot)) org-src-lang-modes))
               '())
         ,@(if (member "python" load-language-list)
               `((setq org-babel-python-command "python3")) ;TODO absolute path?
               '()))
        ,@(if (get-value 'emacs-eval-in-repl config)
              `((eval-when-compile (require 'org-babel-eval-in-repl))
                 (with-eval-after-load
                  'org
                  (defun org-meta-return-around (org-fun &rest args)
                    "Run `ober-eval-in-repl' if in source code block,
`ober-eval-block-in-repl' if at header,
and `org-meta-return' otherwise."
                    (if (org-in-block-p '("src"))
                        (let* ((point (point))
                               (element (org-element-at-point))
                               (area (org-src--contents-area element))
                               (beg (copy-marker (nth 0 area))))
                          (if (< point beg)
                              (ober-eval-block-in-repl)
                              (ober-eval-in-repl)))
                        (apply org-fun args)))
                  (advice-add 'org-meta-return :around 'org-meta-return-around))
                 ;; (define-key org-mode-map (kbd "C-<return>") 'ober-eval-in-repl)
                 ;; (define-key org-mode-map (kbd "M-<return>") 'ober-eval-block-in-repl)
                 )
              '())
        (require 'org-tempo)
        ,@(if block-templates?
              ;; <sh Tab to expand template
              `((with-eval-after-load
                 'org
                 (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
                 (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
                 (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
                 (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
                 (add-to-list 'org-structure-template-alist '("py" . "src python"))
                 (add-to-list 'org-structure-template-alist '("go" . "src go"))
                 (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
                 (add-to-list 'org-structure-template-alist '("json" . "src json"))))
              '()))
      #:elisp-packages
      (append
       (if (member "dot" load-language-list) (list emacs-graphviz-dot-mode) '())
       (if (get-value 'emacs-eval-in-repl config)
           (list emacs-org-babel-eval-in-repl) '()))
     #:summary "\
Emacs Org Babel configuration"
     #:commentary "\
")))

  (feature
   (name f-name)
   (values `((,f-name . 'emacs-org-babel)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-eval-in-repl
          #:key
          (load-language-list (list "emacs-lisp"))
          (repl-placement "left")
          (rely-on-geiser? #t)
          (emacs-eval-in-repl emacs-eval-in-repl)
          (emacs-eval-in-repl-shell emacs-eval-in-repl-shell)
          (emacs-eval-in-repl-python emacs-eval-in-repl-python)
          (emacs-eval-in-repl-geiser emacs-eval-in-repl-geiser)
          (emacs-eval-in-repl-ielm emacs-eval-in-repl-ielm))
  "Configure eval-in-repl for emacs."

  (define emacs-f-name 'eval-in-repl)
  (define f-name (symbol-append 'emacs- emacs-f-name))
  (ensure-pred list? load-language-list)
  (ensure-pred string? repl-placement)
  (ensure-pred boolean? rely-on-geiser?)

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile (require 'eval-in-repl))
        (setq eir-repl-placement ',(string->symbol repl-placement))

        ,@(if (member "emacs-lisp" load-language-list)
              `((eval-when-compile (require 'eval-in-repl-ielm))
                ;; Evaluate expression in the current buffer.
                (setq eir-ielm-eval-in-current-buffer t)
                ;; for .el files
                (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
                ;; for *scratch*
                (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
                ;; for M-x info
                ;; (define-key Info-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
                )
              '())

        ,@(if (member "python" load-language-list)
              `((eval-when-compile (require 'eval-in-repl-python))
                (add-hook 'python-mode-hook
                          '(lambda ()
                             (local-set-key (kbd "<C-return>") 'eir-eval-in-python))))
              '())

        ,@(if (member "shell" load-language-list)
              `((eval-when-compile (require 'eval-in-repl-shell))
                (add-hook 'sh-mode-hook
                          '(lambda()
                             (local-set-key (kbd "C-<return>") 'eir-eval-in-shell)))
                ;; Version with opposite behavior to eir-jump-after-eval configuration
                (defun eir-eval-in-shell2 ()
                  "eval-in-repl for shell script (opposite behavior)

This version has the opposite behavior to the eir-jump-after-eval
configuration when invoked to evaluate a line."
                  (interactive)
                  (let ((eir-jump-after-eval (not eir-jump-after-eval)))
                    (eir-eval-in-shell)))
                (add-hook 'sh-mode-hook
                          '(lambda()
                             (local-set-key (kbd "C-M-<return>") 'eir-eval-in-shell2))))
              '())

        ,@(if (and rely-on-geiser? (or (member "racket" load-language-list)
                                       (member "guile" load-language-list)
                                       (member "scheme" load-language-list)))
              `((eval-when-compile (require 'eval-in-repl-geiser))
                (add-hook 'geiser-mode-hook
		          '(lambda ()
		             (local-set-key (kbd "<C-return>") 'eir-eval-in-geiser))))
              '())

        (with-eval-after-load
         'eval-in-repl
         (setq eir-jump-after-eval nil)))
     #:elisp-packages
     (list emacs-eval-in-repl
           emacs-eval-in-repl-shell emacs-eval-in-repl-python
           emacs-eval-in-repl-geiser emacs-eval-in-repl-ielm)
     #:summary "\
Partial emacs eval-in-repl configuration"
     #:commentary "\
")))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-eval-in-repl)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-guix-development
          #:key
          (guix-load-path "~/src/guix")
          (other-guile-load-paths '()))
  "Configure emacs for guix development."
  ;; FIXME Both guix-load-path and other-guile-load-paths
  ;; need to be absolute without ~ to work properly.
  (ensure-pred string? guix-load-path)
  (ensure-pred list? other-guile-load-paths)

  (define emacs-f-name 'guix-development)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (guix-tempel-snippet mode hash)
    (let ((version "1.4.0rc1")
          (commit "020184fd39c6244e0336db3c608d3946b8d20490")
          (revision 0))
      (package
        (inherit (current-guix))
        (version (string-append version "-"
                                (number->string revision)
                                "." (string-take commit 7)))
        (source
         (origin
           (method url-fetch)
           (uri (string-append
                 "https://git.savannah.gnu.org/cgit/guix.git/"
                 "plain/etc/snippets/tempel/" mode "-mode"
                 "?id=" commit))
           (file-name (string-append "guix-tempel-snippet-" mode))
           (sha256 (base32 hash))))
        (build-system copy-build-system)
        (arguments '()))))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `(;; Geiser
        ;; (with-eval-after-load
        ;;  'geiser-guile
        ;;  ,@(cons*
        ;;     (map
        ;;      (lambda (guile-load-path)
        ;;        `(add-to-list 'geiser-guile-load-path ,guile-load-path))
        ;;      (append (list guix-load-path) other-guile-load-paths))))
        ;; Commit snippets
        ,@(if emacs-tempel
              `((with-eval-after-load
                 'tempel
                 (if (stringp tempel-path)
                     (setq tempel-path (list tempel-path)))
                 (add-to-list
                  'tempel-path
                  ,(file-append
                    (guix-tempel-snippet
                     "scheme"
                     "0gsqsss92k02qr9xsb1nagp29i8by2bxzk4cqacx9cg31rqmrpqx")
                    "/guix-tempel-snippet-scheme"))
                 (add-to-list
                  'tempel-path
                  ,(file-append
                    (guix-tempel-snippet
                     "text"
                     "1qf82rldzznj2g79pjbc2g8npqpxjcpsfvw7vvwrz7869rmh7ksy")
                    "/guix-tempel-snippet-text"))))
              '())
        ;; Copyright
        (setq copyright-names-regexp
              (format "%s <%s>" user-full-name user-mail-address)))
      #:elisp-packages '()
      #:summary "\
Configure emacs for guix development, ensure the Perfect Setup as detailed in\
the Guix manual."
      #:commentary "\
Configure geiser with load-paths, yasnippets for commits, and configure\
copyright.")))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

;;; emacs.scm end here
