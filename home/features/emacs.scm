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

  #:export (feature-emacs-evil
            feature-emacs-general
            ))


;;;
;;; rde emacs utilities.
;;;

(define* (feature-emacs-evil
          #:key
          (emacs-evil emacs-evil)
          (emacs-evil-collection emacs-evil-collection)
          (emacs-undo-tree emacs-undo-tree))
  "Configure evil-mode for emacs."

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


        (setq evil-want-keybinding nil)

        (require 'evil)
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

        (require 'undo-tree)
        (global-undo-tree-mode 1)

        (with-eval-after-load
         'evil
         (add-hook 'evil-mode-hook 'rde-evil-hook)
         (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
         (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

         ;; Use visual line motions even outside of visual-line-mode buffers
         (evil-global-set-key 'motion "j" 'evil-next-visual-line)
         (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

         ;; Disable arrow keys in normal and visual modes
         (define-key evil-normal-state-map (kbd "<left>") 'arrow-keys-disabled)
         (define-key evil-normal-state-map (kbd "<right>") 'arrow-keys-disabled)
         (define-key evil-normal-state-map (kbd "<down>") 'arrow-keys-disabled)
         (define-key evil-normal-state-map (kbd "<up>") 'arrow-keys-disabled)
         (evil-global-set-key 'motion (kbd "<left>") 'arrow-keys-disabled)
         (evil-global-set-key 'motion (kbd "<right>") 'arrow-keys-disabled)
         (evil-global-set-key 'motion (kbd "<down>") 'arrow-keys-disabled)
         (evil-global-set-key 'motion (kbd "<up>") 'arrow-keys-disabled)

         (evil-set-initial-state 'messages-buffer-mode 'normal)
         (evil-set-initial-state 'dashboard-mode 'normal)


         (require 'evil-collection)
         (setq evil-collection-company-use-tng nil) ;; Is this a bug in evil-collection?
         (setq evil-collection-outline-bind-tab-p nil))

        (with-eval-after-load
         'evil-collection
         (setq evil-collection-mode-list
               (remove 'lispy evil-collection-mode-list))
         (evil-collection-init)))
      #:elisp-packages (list emacs-evil emacs-evil-collection emacs-undo-tree)
      #:summary "\
Extensible vi layer for Emacs."
      #:commentary "\
Pure copy of my previous configuration, mostly taken from daviwil.
")))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-evil)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-general
          #:key
          (emacs-general emacs-general)
          (stateful-keymaps #f)
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

        ,@(if stateful-keymaps
              `((require 'hydra))
              '())

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

;;; emacs.scm end here
