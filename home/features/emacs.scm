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

  #:export (feature-emacs-evil
            feature-emacs-ui
            feature-emacs-ux
            feature-emacs-tramp
            feature-emacs-orderless
            feature-emacs-parinfer
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
         (setq evil-collection-outline-bind-tab-p nil))

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
          (show-line-numbers? #f))
  "Small emacs UI tweaks inspired from daviwil's configuration."
  (ensure-pred boolean? show-line-numbers?)

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
        )
      #:elisp-packages '()
      #:summary "\
Small emacs UI tweaks inspired from daviwil's configuration.
"
      )))

  (feature
   (name f-name)
   (values `((,f-name . 'emacs-ui)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-ux
          #:key
          (unwarn? #f)
          (auto-save? #f)
          (auto-update-buffers? #f)
          (auto-clean-space? #f))
  "Small emacs UX tweaks inspired from daviwil's configuration."
  (ensure-pred boolean? unwarn?)
  (ensure-pred boolean? auto-save?)
  (ensure-pred boolean? auto-update-buffers?)
  (ensure-pred boolean? auto-clean-space?)

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
              '())
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
        )
      #:elisp-packages (append (if auto-clean-space? (list emacs-ws-butler) '())
                               (if auto-save? (list emacs-super-save) '()))
      #:summary "\
Small emacs UX tweaks inspired from daviwil's configuration.
"
      )))

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
