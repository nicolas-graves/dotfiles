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

  #:export (feature-emacs-evil))


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

        (require 'evil)
        (evil-mode 1)
        (setq evil-want-integration t)
        (setq evil-want-keybinding nil)
        (setq evil-want-C-u-scroll t)
        (setq evil-want-C-i-jump nil)
        (setq evil-respect-visual-line-mode t)
        (setq evil-undo-system 'undo-tree)

        ;; Since =evil-mode= take over =C-u= for buffer scrolling,
        ;; the =universal-argument= command needs to be rebind to another key
        ;; sequence, here =C-M-u=.
        (global-set-key (kbd "C-M-u") 'universal-argument)

        (require 'evil-collection)
        (setq evil-collection-company-use-tng nil) ;; Is this a bug in evil-collection?
        (setq evil-collection-outline-bind-tab-p nil)

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
         (evil-set-initial-state 'dashboard-mode 'normal))

        (with-eval-after-load
         'evil-collection
         (setq evil-collection-mode-list
               (remove 'lispy evil-collection-mode-list))
         (evil-collection-init)))
      #:elisp-packages (list emacs-evil emacs-evil-collection)
      #:summary "\
Extensible vi layer for Emacs."
      #:commentary "")))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-evil)))
   (home-services-getter get-home-services)))

;;; emacs.scm end here
