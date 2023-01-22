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

  #:use-module (packages emacs)

  #:export (feature-emacs-evil
            feature-emacs-saving
            feature-emacs-lispy
            feature-emacs-flycheck
            feature-emacs-web-mode
            feature-emacs-guix-development
            feature-emacs-dired-hacks
            feature-emacs-org-babel
            feature-emacs-python
            feature-emacs-eval-in-repl
            feature-emacs-app-launcher))


;;;
;;; rde emacs utilities.
;;;

(define* (feature-emacs-evil
          #:key
          (emacs-evil emacs-evil)
          (emacs-evil-collection emacs-evil-collection)
          (emacs-evil-org emacs-evil-org)
          (emacs-undo-fu emacs-undo-fu)
          (stateful-keymaps? #f)
          (nerd-commenter? #f)
          (disable-arrow-keys? #f))
  "Configure evil-mode for emacs."
  (ensure-pred boolean? stateful-keymaps?)
  (ensure-pred boolean? nerd-commenter?)
  (ensure-pred boolean? disable-arrow-keys?)

  (define emacs-f-name 'evil)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((defun rde-evil-hook ()
          (dolist (mode '(custom-mode
                          eshell-mode
                          git-rebase-mode
                          term-mode))
                  (add-to-list 'evil-emacs-state-modes mode)))

        (eval-when-compile
         (require 'evil)
         (require 'evil-collection)
         (require 'undo-fu))

        (setq evil-want-keybinding nil)

        (require 'evil-collection-autoloads)
        (with-eval-after-load
         'evil-collection-autoloads
         (evil-collection-init))

        (require 'evil-autoloads)
        (with-eval-after-load
         'evil-autoloads
         (evil-mode 1))

        (setq evil-want-integration t)
        (setq evil-want-C-u-scroll t)
        (setq evil-want-C-i-jump nil)
        (setq evil-respect-visual-line-mode t)
        (setq evil-undo-system 'undo-fu)

        ;; Since =evil-mode= take over =C-u= for buffer scrolling,
        ;; the =universal-argument= command needs to be rebind to another key
        ;; sequence, here =C-M-u=.
        (global-unset-key (kbd "C-M-u"))
        (global-unset-key (kbd "C-u"))
        (global-set-key (kbd "C-M-u") 'universal-argument)
        (global-set-key (kbd "C-u") 'evil-scroll-up)
        ;; Keybinding preferences
        (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
        (global-set-key (kbd "<lwindow-j>") 'ignore)
        (global-set-key (kbd "<lwindow-k>") 'ignore)
        (global-set-key (kbd "s-?") 'embark-bindings)
        (global-set-key (kbd "C-x b") 'consult-buffer)
        (global-set-key (kbd "C-M-l") 'consult-imenu)

        ;; TODO define in base defaults
        (setq tab-width 2)
        (setq evil-shift-width tab-width)

        (add-hook
         'post-command-hook
         '(lambda ()
            (let ((color (if (equal custom-enabled-themes
                                    (cons 'modus-vivendi '()))
                             "white"
                             "black")))
              (setq evil-default-cursor color)
              (setq evil-normal-state-cursor `(,color box))
              (setq evil-emacs-state-cursor color)
              (setq evil-motion-state-cursor color))))

        ;; use evil in minibuffers with Ctrl key.
        ;;(setq evil-want-minibuffer t) ; don't understand what it does
        (let ((map minibuffer-mode-map))
          (define-key map (kbd "C-j") 'next-line-or-history-element)
          (define-key map (kbd "C-k") 'previous-line-or-history-element)
          (define-key map (kbd "C-r") 'consult-history))

        (with-eval-after-load
         'vertico
         (let ((map vertico-map))
           (define-key map (kbd "C-j") 'vertico-next)
           (define-key map (kbd "C-k") 'vertico-previous)))

        ,@(if stateful-keymaps?
              `((eval-when-compile (require 'hydra)))
              '())

        ,@(if nerd-commenter?
              `((eval-when-compile (require 'evil-nerd-commenter)))
              '())

        (with-eval-after-load
         'evil
         (add-hook 'evil-mode-hook 'rde-evil-hook)
         (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
         (define-key evil-insert-state-map
           (kbd "C-h") 'evil-delete-backward-char-and-join)

         ;; Use visual line motions even outside of visual-line-mode buffers
         (evil-global-set-key 'motion "j" 'evil-next-visual-line)
         (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

         ;; Disable arrow keys in normal and visual modes

         ,@(if disable-arrow-keys?
               `((defun arrow-keys-disabled ()
                   (interactive)
                   (message "Arrow keys disabled."))

                 (let ((map evil-normal-state-map))
                   (define-key map (kbd "<left>") 'arrow-keys-disabled)
                   (define-key map (kbd "<right>") 'arrow-keys-disabled)
                   (define-key map (kbd "<down>") 'arrow-keys-disabled)
                   (define-key map (kbd "<up>") 'arrow-keys-disabled))
                 (evil-global-set-key 'motion (kbd "<left>") 'arrow-keys-disabled)
                 (evil-global-set-key 'motion (kbd "<right>") 'arrow-keys-disabled)
                 (evil-global-set-key 'motion (kbd "<down>") 'arrow-keys-disabled)
                 (evil-global-set-key 'motion (kbd "<up>") 'arrow-keys-disabled))
               '())

         (evil-define-key '(normal insert visual)
                          org-mode-map (kbd "C-j") 'org-next-visible-heading)
         (evil-define-key '(normal insert visual)
                          org-mode-map (kbd "C-k")
                          'org-previous-visible-heading)
         (evil-define-key '(normal insert visual)
                          org-mode-map (kbd "M-j") 'org-metadown)
         (evil-define-key '(normal insert visual)
                          org-mode-map (kbd "M-k") 'org-metaup)

         (evil-set-initial-state 'messages-buffer-mode 'normal)
         (evil-set-initial-state 'dashboard-mode 'normal)

         ;; Is this a bug in evil-collection?
         (setq evil-collection-company-use-tng nil)
         (setq evil-collection-outline-bind-tab-p nil)

         (with-eval-after-load
          'winner
          (let ((map evil-window-map))
            (define-key map (kbd "u") 'winner-undo)
            (define-key map (kbd "U") 'winner-redo))))

        (with-eval-after-load
         'evil-collection
         (setq evil-collection-mode-list
               (remove 'lispy evil-collection-mode-list)))

        (add-hook 'org-mode-hook 'evil-org-mode)
        (add-hook 'org-agenda-mode-hook 'evil-org-mode)
        (with-eval-after-load
         'org
         (add-hook 'evil-org-mode-hook
                   (lambda ()
                     (evil-org-set-key-theme
                      '(navigation todo insert textobjects additional))))
         (with-eval-after-load
          'evil-org
          (require 'evil-org-agenda)
          (evil-org-agenda-set-keys))))
      #:elisp-packages (append (if stateful-keymaps? (list emacs-hydra) '())
                               (if nerd-commenter?
                                   (list emacs-evil-nerd-commenter) '())
                               (list emacs-evil emacs-evil-collection
                                     emacs-evil-org emacs-undo-fu))
      #:summary "\
Extensible vi layer for Emacs."
      #:commentary "\
Adapted from Nicolas Graves' previous configuration, mostly taken from daviwil.
")))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-evil)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-saving
          #:key
          (emacs-super-save emacs-super-save)
          (emacs-undo-fu-session emacs-undo-fu-session))
  "Emacs defaults to help with data save and recovery."
  (ensure-pred file-like? emacs-super-save)
  (ensure-pred file-like? emacs-undo-fu-session)

  (define emacs-f-name 'saving)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile (require 'super-save) (require 'undo-fu-session))
        (setq super-save-mode t)
        (with-eval-after-load
         'super-save
         (setq super-save-auto-save-when-idle t))
        (setq global-undo-fu-session-mode t)
        (setq undo-fu-session-compression 'gz)
        (setq undo-fu-session-file-limit 1000))
     #:elisp-packages (list emacs-super-save emacs-undo-fu-session)
     #:summary "\
Emacs defaults to help with data save and recovery."
     #:commentary "\
Save Emacs buffers when they lose focus and save and recover undo steps
between Emacs sessions.")))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

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

(define* (feature-emacs-lispy
          #:key
          (emacs-lispy emacs-lispy))
  "configure lispy for emacs."

  (define emacs-f-name 'lispy)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile (require 'lispy))
        (add-hook 'emacs-lisp-mode 'lispy-mode)
        (add-hook 'scheme-mode 'lispy-mode)
        (add-hook 'lispy-mode 'lispyville-mode)
        (with-eval-after-load
         'lispyville
         (lispyville-set-key-theme
          '(operators c-w additional
                      additional-movement slurp/barf-cp
                      prettify))))
      #:elisp-packages (list emacs-lispy emacs-lispyville)
      #:summary "\
lispy"
      #:commentary "\
")))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-lispy)))
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

(define* (feature-emacs-dired-hacks
          #:key
          (emacs-all-the-icons-dired emacs-all-the-icons-dired)
          (emacs-dired-hacks emacs-dired-hacks)
          (evil? #f))
  "Configure dired, the Emacs' directory browser and editor."
  (define emacs-f-name 'dired-hacks)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile (require 'all-the-icons-dired))
        (with-eval-after-load
         'dired
         (setq dired-listing-switches "-agho --group-directories-first"
               dired-omit-files "^\\.[^.].*"
               dired-omit-verbose nil
               dired-hide-details-hide-symlink-targets nil
               dired-kill-when-opening-new-dired-buffer t
               delete-by-moving-to-trash t)

         (autoload 'dired-omit-mode "dired-x")

         (add-hook 'dired-load-hook
                   (lambda ()
                     (interactive)
                     (dired-collapse)))

         (add-hook 'dired-mode-hook
                   (lambda ()
                     (interactive)
                     (dired-omit-mode 1)
                     (dired-hide-details-mode 1)
                     (all-the-icons-dired-mode 1))
                   (hl-line-mode 1))

         (eval-when-compile
          (require 'dired-rainbow)
          (require 'dired-ranger)
          (require 'dired-collapse))

         (with-eval-after-load
          'dired-rainbow
          (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
          (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
          (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
          (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
          (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
          (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
          (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
          (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
          (dired-rainbow-define log "#c17d11" ("log"))
          (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
          (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
          (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
          (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
          (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
          (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
          (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
          (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
          (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
          (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
          (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

         ,@(if evil?
               '((evil-collection-define-key
                  'normal 'dired-mode-map
                  "h" 'dired-single-up-directory
                  "H" 'dired-omit-mode
                  "l" 'dired-single-buffer
                  "y" 'dired-ranger-copy
                  "X" 'dired-ranger-move
                  "p" 'dired-ranger-paste))
               '())))
      #:elisp-packages (list emacs-dired-hacks emacs-all-the-icons-dired)
      #:summary "\
Configurations and tweaks for emacs built-in file manager"
      #:commentary "\
Small tweaks, xdg entry for openning directories in emacs client."
      #:keywords '(convenience dired files))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
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
