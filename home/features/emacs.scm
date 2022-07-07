;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2022 Samuel Culpepper <samuel@samuelculpepper.com>
;;; Copyright © 2022 Demis Balbach <db@minikn.xyz>
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

(define-module (home features emacs)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
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

  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix transformations)

  #:use-module (ngraves packages emacs)
  #:use-module (guixrus packages emacs)

  #:export (feature-emacs-evil
            feature-emacs-ui
            feature-emacs-ux
            feature-emacs-tramp
            feature-emacs-orderless
            feature-emacs-parinfer
            feature-emacs-guix-development
            feature-emacs-dired-hacks
            feature-emacs-org-babel
            feature-emacs-eval-in-repl
            feature-emacs-origami-el))


;;;
;;; rde emacs utilities.
;;;

(define* (feature-emacs-evil
          #:key
          (emacs-evil emacs-evil)
          (emacs-evil-collection emacs-evil-collection)
          (emacs-undo-tree emacs-undo-tree)
          (stateful-keymaps? #f)
          (nerd-commenter? #f))
  "Configure evil-mode for emacs."
  (ensure-pred boolean? stateful-keymaps?)
  (ensure-pred boolean? nerd-commenter?)

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

        (defun arrow-keys-disabled ()
          (interactive)
          (message "Arrow keys disabled."))

        (eval-when-compile
         (require 'evil)
         (require 'undo-tree)
         (require 'evil-collection))

        (setq evil-want-keybinding nil)

        (evil-mode 1)
        (setq evil-want-integration t)
        (setq evil-want-C-u-scroll t)
        (setq evil-want-C-i-jump nil)
        (setq evil-respect-visual-line-mode t)
        (setq evil-undo-system 'undo-tree)

        ;; Since =evil-mode= take over =C-u= for buffer scrolling,
        ;; the =universal-argument= command needs to be rebind to another key
        ;; sequence, here =C-M-u=.
        (global-set-key (kbd "C-M-u") 'universal-argument)
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
         (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

         ;; Use visual line motions even outside of visual-line-mode buffers
         (evil-global-set-key 'motion "j" 'evil-next-visual-line)
         (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

         ;; Disable arrow keys in normal and visual modes
         (let ((map evil-normal-state-map))
           (define-key map (kbd "<left>") 'arrow-keys-disabled)
           (define-key map (kbd "<right>") 'arrow-keys-disabled)
           (define-key map (kbd "<down>") 'arrow-keys-disabled)
           (define-key map (kbd "<up>") 'arrow-keys-disabled))
         (evil-global-set-key 'motion (kbd "<left>") 'arrow-keys-disabled)
         (evil-global-set-key 'motion (kbd "<right>") 'arrow-keys-disabled)
         (evil-global-set-key 'motion (kbd "<down>") 'arrow-keys-disabled)
         (evil-global-set-key 'motion (kbd "<up>") 'arrow-keys-disabled)

         (evil-set-initial-state 'messages-buffer-mode 'normal)
         (evil-set-initial-state 'dashboard-mode 'normal)

         (global-undo-tree-mode 1)

         (evil-collection-init)
         (setq evil-collection-company-use-tng nil) ;; Is this a bug in evil-collection?
         (setq evil-collection-outline-bind-tab-p nil)

         (with-eval-after-load
          'winner
          (let ((map evil-window-map))
            (define-key map (kbd "u") 'winner-undo)
            (define-key map (kbd "U") 'winner-redo))))

        (with-eval-after-load
         'evil-collection
         (setq evil-collection-mode-list
               (remove 'lispy evil-collection-mode-list))))
      #:elisp-packages (append (if stateful-keymaps? (list emacs-hydra) '())
                               (if nerd-commenter? (list emacs-evil-nerd-commenter) '())
                               (list emacs-evil emacs-evil-collection emacs-undo-tree))
      #:summary "\
Extensible vi layer for Emacs."
      #:commentary "\
Adapted from Nicolas Graves' previous configuration, mostly taken from daviwil.
")))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-evil)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-ui
          #:key
          (show-line-numbers? #f)
          (org-mode-margins? #f))
  "Small emacs UI tweaks inspired from daviwil's configuration."
  (ensure-pred boolean? show-line-numbers?)
  (ensure-pred boolean? org-mode-margins?)

  (define emacs-f-name 'ui)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `(,@(if show-line-numbers?
              `((column-number-mode)
                ;; Enable line numbers for some modes
                (dolist (mode '(text-mode-hook
                                prog-mode-hook
                                conf-mode-hook))
                        (add-hook
                         mode (lambda () (display-line-numbers-mode 1))))
                ;; Override some modes which derive from the above
                (dolist (mode '(org-mode-hook))
                        (add-hook
                         mode (lambda () (display-line-numbers-mode 0)))))
              '())
        ,@(if org-mode-margins?
              `((defun rde-org-mode-visual-fill ()
                  (setq visual-fill-column-width 110
                        visual-fill-column-center-text t)
                  (visual-fill-column-mode 1))
                (add-hook 'org-mode-hook 'rde-org-mode-visual-fill))
              '()))
      #:elisp-packages (if org-mode-margins? (list emacs-visual-fill-column) '())
      #:summary "\
Small emacs UI tweaks inspired from daviwil's configuration.
")))


  (feature
   (name f-name)
   (values `((,f-name . 'emacs-ui)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-ux
          #:key
          (unwarn? #f)
          (auto-save? #f)
          (auto-update-buffers? #f)
          (auto-clean-space? #f)
          (control-text-scale? #f)
          (control-buffer-placement? #f))
  "Small emacs UX tweaks inspired from daviwil's configuration."
  (ensure-pred boolean? unwarn?)
  (ensure-pred boolean? auto-save?)
  (ensure-pred boolean? auto-update-buffers?)
  (ensure-pred boolean? auto-clean-space?)
  (ensure-pred boolean? control-text-scale?)
  (ensure-pred boolean? control-buffer-placement?)

  (define emacs-f-name 'ux)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `(,@(if unwarn?
              `(;; Don't warn for large files
                (setq large-file-warning-threshold nil)
                ;; Don't warn for followed symlinked files
                (setq vc-follow-symlinks t)
                ;; Don't warn when advice is added for functions
                (setq ad-redefinition-action 'accept))
              )
        ,@(if auto-save?
              `((require 'super-save)
                (super-save-mode 1)
                (eval-after-load
                 'super-save
                 (setq super-save-auto-save-when-idle t)))
              '())
        ,@(if auto-update-buffers?
              `(;; Revert Dired and other buffers
                (setq global-auto-revert-non-file-buffers t)
                ;; Revert buffers when the underlying file has changed
                (global-auto-revert-mode 1))
              '())
        ,@(if auto-clean-space?
              `((eval-when-compile (require 'ws-butler))
                (add-hook 'text-mode-hook 'ws-butler-mode)
                (add-hook 'prog-mode-hook 'ws-butler-mode))
              '())
        ,@(if control-text-scale?
              `((eval-when-compile (require 'default-text-scale))
                ;; keybindings =C+M+-= and =C+M+-=
                (default-text-scale-mode))
              '())
        ,@(if control-buffer-placement?
              `((setq display-buffer-base-action
                      '(display-buffer-reuse-mode-window
                        display-buffer-reuse-window
                        display-buffer-same-window))
                ;; If a popup does happen, don't resize windows to be equal-sized
                (setq even-window-sizes nil))
              '()))

      #:elisp-packages (append (if auto-clean-space? (list emacs-ws-butler) '())
                               (if control-text-scale? (list emacs-default-text-scale) '())
                               (if auto-save? (list emacs-super-save) '()))
      #:summary "\
Small emacs UX tweaks inspired from daviwil's configuration.
")))


  (feature
   (name f-name)
   (values `((,f-name . 'emacs-ux)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-general
          #:key
          (emacs-general emacs-general)
          (files-shortcuts #f))
  "Configure general.el for emacs. Also added files-shortcuts and hydra for
stateful keymaps."

  (define emacs-f-name 'general)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((require 'general)
        (general-evil-setup t)

        (general-create-definer rde-leader-key-def
         :keymaps '(normal insert visual emacs)
         :prefix "SPC"
         :global-prefix "C-SPC")

        (general-create-definer rde-ctrl-c-keys
                                :prefix "C-c")

        ,@(if files-shortcuts
              `(;; helpers
                (defun rde-org-file-jump-to-heading (org-file heading-title)
                  (interactive)
                  (find-file (expand-file-name org-file))
                  (goto-char (point-min))
                  (search-forward (concat "* " heading-title))
                  (org-overview)
                  (org-reveal)
                  (org-show-subtree)
                  (forward-line))

                (defun rde-org-file-show-headings (org-file)
                  (interactive)
                  (find-file (expand-file-name org-file))
                  (counsel-org-goto)
                  (org-overview)
                  (org-reveal)
                  (org-show-subtree)
                  (forward-line))

                ;; bindings
                ;; FIXME these bindings need feature ivy

                (rde-leader-key-def
                 "fn" '((lambda ()
                          (interactive) (counsel-find-file "~/archives/journal/"))
                        :which-key "notes")
                 "fd"  '(:ignore t :which-key "dotfiles")
                 "fdf" '((lambda ()
                           (interactive) (counsel-find-file "~/.dotfiles/"))
                         :which-key "dotfiles")
                 "fdc" '((lambda ()
                           (interactive)
                           (counsel-find-file "~/.dotfiles/home/yggdrasil/files/config"))
                         :which-key "legacy config")
                 "fc" '((lambda ()
                           (interactive)
                           (find-file (expand-file-name "~/.dotfiles/config.org")))
                         :which-key "config")
                 "fC" '((lambda ()
                          (interactive)
                          (rde-org-file-show-headings "~/.dotfiles/config.org"))
                        :which-key "config")
                 "fb" '((lambda ()
                          (interactive) (find-file "~/resources/roam/biblio.bib"))
                        :which-key "biblio")
                 "fs" '((lambda ()
                          (interactive) (counsel-find-file "~/.local/src/"))
                        :which-key "source")
                 ))
              '())
        )
      #:elisp-packages (if stateful-keymaps (list emacs-general emacs-hydra)
                           (list emacs-general))
      #:summary "\
general.el is a fantastic library for defining prefixed keybindings,\
especially in conjonction with Evil modes. Including hydra for stateful\
keymaps."
      #:commentary "\
Pure copy of my previous configuration, mostly taken from daviwil.
")))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-general)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-tramp
          #:key
          (emacs-tramp emacs-tramp))
  "Configure tramp for emacs."

  (define emacs-f-name 'tramp)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile (require 'tramp))
        (with-eval-after-load
         'tramp
         (setq tramp-default-method "ssh")
         ;; Make sure tramp works on remote guix machines
         (add-to-list 'tramp-remote-path 'tramp-own-remote-path)))
      #:elisp-packages (list emacs-tramp)
      #:summary "\
TRAMP"
      #:commentary "\
")))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-tramp)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-org-babel
          #:key
          (load-language-list (list "emacs-lisp"))
          ;; (eval-in-repl? #f)
          )
  "Configure org-babel for emacs."

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
               '())
         ;; ,@(if eval-in-repl?
         ;;       `((with-eval-after-load
         ;;          'ob
         ;;          (require 'org-babel-eval-in-repl)
         ;;          (define-key org-mode-map (kbd "C-<return>") 'ober-eval-in-repl)
         ;;          (define-key org-mode-map (kbd "M-<return>") 'ober-eval-block-in-repl)))
         ;;       '())
         )))
     #:elisp-packages (if #f (list emacs-org-babel-eval-in-repl) '())
     #:summary "\
Emacs Org Babel configuration"
     #:commentary "\
"))

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
      `((require 'eval-in-repl)

        (setq eir-repl-placement ,(string->symbol repl-placement))

        ,@(if (member "emacs-lisp" load-language-list)
              `((eval-when-compile (require 'eval-in-repl-ielm))
                ;; Evaluate expression in the current buffer.
                (setq eir-ielm-eval-in-current-buffer t)
                ;; for .el files
                (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
                ;; for *scratch*
                (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
                ;; for M-x info
                (define-key Info-mode-map (kbd "<C-return>") 'eir-eval-in-ielm))
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
         (setq eir-jump-after-eval nil))))
     #:elisp-packages
     (list emacs-eval-in-repl
           emacs-eval-in-repl-shell emacs-eval-in-repl-python
           emacs-eval-in-repl-geiser emacs-eval-in-repl-ielm)
     #:summary "\
Partial emacs eval-in-repl configuration"
     #:commentary "\
"))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-eval-in-repl)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-origami-el
          #:key
          (emacs-origami-el emacs-origami-el))
  "Configure origami-el for emacs."

  (define emacs-f-name 'origami-el)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile (require 'origami))
        (with-eval-after-load
         'origami
         (add-hook 'yaml-mode-hook 'origami-mode)))
      #:elisp-packages (list emacs-origami-el)
      #:summary "\
Small package for folding."
      #:commentary "\
")))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-origami-el)))
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

(define* (feature-emacs-orderless
          #:key
          (emacs-orderless emacs-orderless))
  "Configure orderless completion for emacs."

  (define emacs-f-name 'orderless)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile (require 'orderless))
        (setq completion-styles '(orderless)
              completion-category-defaults nil
              completion-category-overrides '((file (styles . (partial-completion))))))
      #:elisp-packages (list emacs-orderless)
      #:summary "\
orderless"
      #:commentary "\
")))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-orderless)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-guix-development
          #:key
          (guix-load-path "~/src/guix")
          (other-guile-load-paths '())
          (yasnippet-installed? #f))
  "Configure emacs for guix development."
  (ensure-pred string? guix-load-path)
  ;; (ensure-pred list? other-guile-load-paths)
  (ensure-pred boolean? yasnippet-installed?)

  (define emacs-f-name 'guix-development)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((with-eval-after-load
         'geiser-guile
         ,@(cons*
            (map
             (lambda (guile-load-path)
               `(add-to-list 'geiser-guile-load-path ,guile-load-path))
             (append (list guix-load-path) other-guile-load-paths))))
        ,@(if yasnippet-installed?
              `((with-eval-after-load
                 'yasnippet
                 (add-to-list
                  'yas-snippet-dirs ,(string-append guix-load-path "/etc/snippets"))))
              '())
        (load-file ,(string-append guix-load-path "/etc/copyright.el"))
        (global-set-key (kbd "s-G") 'guix))
      #:elisp-packages '()
      #:summary "\
Configure emacs for guix development."
      #:commentary "\
")))

  (feature
   (name f-name)
   (values `((,f-name . 'emacs-guix-development)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-parinfer
          #:key
          (emacs-parinfer-mode emacs-parinfer-mode))
  "Configure parinfer for emacs."

  (define emacs-f-name 'parinfer)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile (require 'parinfer))
        (add-hook 'clojure-mode-hook 'parinfer-mode)
        (add-hook 'emacs-lisp-mode-hook 'parinfer-mode)
        (add-hook 'common-lisp-mode-hook 'parinfer-mode)
        (add-hook 'scheme-mode-hook 'parinfer-mode)
        (add-hook 'lisp-mode-hook 'parinfer-mode)
        (with-eval-after-load
         'parinfer
         (setq parinfer-extensions
               '(defaults       ; should be included.
                  pretty-parens  ; different paren styles for different modes.
                  evil           ; If you use Evil.
                  smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
                  smart-yank))))
      #:elisp-packages (list emacs-parinfer-mode)
      #:summary "\
parinfer"
      #:commentary "\
")))

  (feature
   (name f-name)
   (values `((,f-name . 'emacs-parinfer)))
   (home-services-getter get-home-services)))

;;; emacs.scm end here
