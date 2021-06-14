;; -*- lexical-binding: t; eval: (outline-minor-mode t) -*-

;;; use-package setup

(eval-and-compile
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t)
  (setq use-package-hook-name-suffix nil))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)


;;; general settings

(use-package emacs
  :custom
  (user-full-name "Nikita Domnitskii")
  (user-mail-address "nikita@domnitskii.me")
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (inhibit-startup-echo-area-message t)
  (initial-major-mode #'org-mode)
  (indent-tabs-mode nil)
  (scroll-step 1)
  (scroll-conservatively 10000)
  (auto-window-vscroll nil)
  (confirm-kill-emacs #'yes-or-no-p)
  (confirm-nonexistent-file-or-buffer nil)
  (require-final-newline t)
  :init
  (advice-add 'yes-or-no-p :override #'y-or-n-p))


(use-package gcmh
  :demand t
  :custom
  (gcmh-mode t))


(use-package files
  :hook
  (before-save-hook . delete-trailing-whitespace))


(use-package so-long
  :defer t
  :custom
  (global-so-long-mode t))


(use-package saveplace
  :defer t
  :custom
  (save-place-mode t))


(use-package savehist
  :defer t
  :custom
  (history-delete-duplicates t)
  (savehist-mode t))


(use-package cus-edit
  :defer t
  :custom
  (custom-file null-device))


(use-package compile
  :defer t
  :custom
  (compilation-scroll-output t))


(use-package project
  :defer t
  :custom
  (project-switch-commands '((?f "Find file" project-find-file)
                             (?g "Find regexp" rg-project)
                             (?e "Eshell" project-eshell)
                             (?v "Magit" magit-project-status))))


(use-package helpful
  :bind
  (([remap describe-function] . helpful-callable)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-key] . helpful-key)
   :map help-map
   ("d" . helpful-at-point)))


(use-package which-key
  :defer t
  :custom
  (which-key-mode t))


(use-package help
  :defer t
  :custom
  (help-window-select t))


(use-package uniquify
  :defer t
  :custom
  (uniquify-after-kill-buffer-p t)
  (uniquify-strip-common-suffix t)
  (uniquify-buffer-name-style 'forward))


(use-package simple
  :defer t
  :custom
  (column-number-indicator-zero-based nil)
  (column-number-mode t))


(use-package paren
  :custom
  (show-paren-style 'parenthesis)
  :hook
  (prog-mode-hook . show-paren-mode))


(use-package hideshow
  :hook
  (conf-mode-hook . hs-minor-mode)
  (prog-mode-hook . hs-minor-mode)
  (text-mode-hook . hs-minor-mode))


(use-package flymake
  :hook
  (prog-mode-hook . flymake-mode)
  :config
  (setq flymake-mode-line-format `(" " ,flymake-mode-line-counters)))


(use-package simple
  :hook
  (prog-mode-hook . visual-line-mode)
  :bind
  (:map ctl-x-map
        ("K" . kill-current-buffer)))


(use-package eldoc
  :defer t
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-mode t))


;;;; evil

(use-package evil
  :custom
  (evil-want-Y-yank-to-eol t)
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-echo-state nil)
  (evil-undo-system 'undo-redo)
  (evil-respect-visual-line-mode t)
  (evil-disable-insert-state-bindings t)
  (evil-want-C-d-scroll nil)
  (evil-want-C-u-scroll nil)
  (evil-want-C-w-delete t)
  :hook
  (after-init-hook . evil-mode))


(use-package evil-collection
  :hook
  (evil-mode-hook . evil-collection-init))


(use-package evil-commentary
  :hook
  (evil-mode-hook . evil-commentary-mode))


(use-package evil-surround
  :hook
  (evil-local-mode-hook . evil-surround-mode))


;;;; ui

(use-package emacs
  :defer t
  :custom
  (use-dialog-box nil)
  (use-file-dialog nil)
  (tooltip-mode nil)
  (menu-bar-mode nil)
  (tool-bar-mode nil))


(use-package scroll-bar
  :defer t
  :custom
  (scroll-bar-mode nil))


(use-package fringe
  :defer t
  :custom
  (fringe-mode 0))


(use-package faces
  :defer t
  :custom-face
  (default ((t (:family "Iosevka" :weight light :height 180))))
  (variable-pitch ((t (:family "Roboto Condensed" :height 180))))
  (fixed-pitch ((t (:inherit default)))))


(use-package modus-themes
  :preface
  (defun modus-checkers (_underline _subtlefg intensefg bg)
    `(:background ,bg :foreground ,intensefg))
  :custom
  (modus-themes-syntax 'alt-syntax-yellow-comments)
  (modus-themes-diffs 'fg-only-deuteranopia)
  (modus-themes-completions nil)
  (modus-themes-mode-line 'borderless-moody)
  (modus-themes-region 'accent-no-extend)
  (modus-themes-org-blocks 'tinted-background)
  (modus-themes-variable-pitch-headings t)
  (modus-themes-paren-match 'intense)
  (modus-themes-hl-line 'accented-background)
  (modus-themes-slanted-constructs t)
  :init
  (advice-add 'modus-themes--lang-check :override #'modus-checkers)
  (load-theme 'modus-operandi t))


(use-package moody
  :custom
  (x-underline-at-descent-line t)
  (moody-mode-line-height 32)
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))


(use-package minions
  :defer t
  :custom
  (minions-direct '(flymake-mode))
  (minions-mode t))


(use-package frame
  :defer t
  :custom
  (blink-cursor-mode nil))


(use-package pulse
  :custom-face
  (pulse-highlight-start-face ((t (:inherit highlight))))
  :hook
  (imenu-after-jump-hook . pulse-line-hook-function)
  :config
  (setq pulse-command-advice-flag t))


;;;; window management

(use-package window
  :defer t
  :custom
  (display-buffer-alist
   `((,(rx "*" (| "H" "h") "elp" (? "ful" (* nonl)) "*")
      (display-buffer-in-side-window)
      (window-height . 0.4)
      (side . bottom)
      (slot . 0))
     (,(rx "*info*")
      (display-buffer-in-side-window)
      (window-height . 0.4)
      (side . bottom)
      (slot . 1))
     (,(rx "*cider-" (| "doc" "error" "test-report") "*")
      (display-buffer-in-side-window)
      (window-height . 0.4)
      (side . bottom)
      (slot . 0))
     (,(rx "*cider-result*")
      (display-buffer-in-side-window)
      (window-height . 0.4)
      (side . bottom)
      (slot . 1))
     (,(rx "*compilation*")
      (display-buffer-in-side-window)
      (window-height . 0.4)
      (side . bottom)
      (slot . -1))
     (,(rx "*" (| "rg" "Occur") "*")
      (display-buffer-in-side-window)
      (window-height . 0.4)
      (side . bottom)
      (slot . 1)))))


;;;; editing

(use-package smartparens
  :hook
  (emacs-lisp-mode-hook . smartparens-strict-mode)
  (scheme-mode-hook . smartparens-strict-mode)
  (clojure-mode-hook . smartparens-strict-mode)
  (clojurec-mode-hook . smartparens-strict-mode)
  (clojurescript-mode-hook . smartparens-strict-mode)
  (lisp-interaction-mode-hook . smartparens-strict-mode)
  (lisp-data-mode-hook . smartparens-strict-mode)
  :config
  (sp-pair "`" nil :actions nil)
  (sp-pair "'" nil :actions nil))


(use-package evil-cleverparens
  :preface
  (defun do-not-map-M-s (f)
    (let ((evil-cp-additional-bindings
           (assoc-delete-all "M-s" evil-cp-additional-bindings)))
      (funcall f)))
  :hook
  (smartparens-strict-mode-hook . evil-cleverparens-mode)
  :init
  (advice-add 'evil-cp-set-additional-bindings :around #'do-not-map-M-s))


;;;; completion

(use-package icomplete
  :defer t
  :custom
  (minibuffer-depth-indicate-mode t)
  (minibuffer-electric-default-mode t)
  (enable-recursive-minibuffers t)
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (resize-mini-windows 'grow-only)
  (icomplete-delay-completions-threshold 0)
  (icomplete-max-delay-chars 0)
  (icomplete-compute-delay 0)
  (icomplete-show-matches-on-no-input t)
  (icomplete-prospects-height 1)
  (completion-cycle-threshold 1)
  (completion-styles '(partial-completion orderless))
  (icomplete-mode t)
  :bind
  (:map icomplete-minibuffer-map
        ("<return>" . minibuffer-force-complete-and-exit)
        ("<tab>" . minibuffer-complete-word)
        ("C-j" . exit-minibuffer)
        ("C-w" . backward-kill-word)
        ("C-u" . backward-kill-sentence)))


(use-package icomplete-vertical
  :after icomplete
  :custom
  (icomplete-vertical-prospects-height 10)
  (icomplete-vertical-mode t)
  :bind
  (:map icomplete-minibuffer-map
        ("C-t" . icomplete-vertical-toggle)))


(use-package marginalia
  :after icomplete
  :custom
  (marginalia-mode t))


(use-package orderless
  :after icomplete
  :preface
  (defun drop-last (s)
    (substring s 0 (1- (length s))))

  (defun orderless-literal-dispatcher (pat _index _total)
    (when (string-suffix-p "=" pat)
      `(orderless-literal . ,(drop-last pat))))

  (defun orderless-sli-dispatcher (pat _index _total)
    (when (string-suffix-p "\\" pat)
      `(orderless-strict-leading-initialism . ,(drop-last pat))))

  :custom
  (orderless-matching-styles '(orderless-flex orderless-regexp))
  (orderless-style-dispatchers '(orderless-literal-dispatcher
                                 orderless-sli-dispatcher)))


(use-package consult
  :after icomplete
  :custom
  (completion-in-region-function 'consult-completion-in-region)
  :config
  (setf (alist-get #'consult-completion-in-region consult-config)
        '(:completion-styles (basic))))


;;;; search and movements

(use-package imenu
  :bind
  (:map goto-map
        ("i" . imenu))
  :custom
  (imenu-auto-rescan t)
  (imenu-auto-rescan-maxout 60000)
  (imenu-use-popup-menu nil)
  (imenu-eager-completion-buffer t)
  :hook
  (imenu-after-jump-hook . recenter))


(use-package flimenu
  :after imenu
  :custom
  (flimenu-global-mode t))


;;;; files
;;;;; dired

(use-package dired
  :commands dired-jump project-dired
  :custom
  (dired-listing-switches "-alh --group-directories-first")
  (dired-omit-files (rx line-start ?. (* nonl)))
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-dwim-target t)
  (dired-auto-revert-buffer t)
  :hook
  (dired-mode-hook . dired-omit-mode)
  (dired-mode-hook . dired-hide-details-mode))


(use-package dired-aux
  :after dired
  :custom
  (dired-create-destination-dirs 'ask)
  (dired-vc-rename-file t))


(use-package dired-async
  :after dired
  :custom
  (dired-async-mode t))


(use-package hl-line
  :hook
  (dired-mode-hook . hl-line-mode))


;;;;; backups and autosaves

(use-package files
  :defer t
  :preface
  (defvar emacs-tmp-dir
    (expand-file-name "emacs" temporary-file-directory))

  (defvar autosave-tmp-dir
    (expand-file-name "autosave" emacs-tmp-dir))

  :custom
  (make-backup-files nil)
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-old-versions 5)
  (kept-new-versions 5)
  (backup-directory-alist `(("." . ,emacs-tmp-dir)))
  (auto-save-default t)
  (auto-save-include-big-deletions t)
  (create-lockfiles nil)
  (auto-save-list-file-prefix autosave-tmp-dir))


;;; language specific stuff
;;;; elisp

(use-package elisp-mode
  :custom
  (eval-expression-print-level nil)
  (eval-expression-print-length nil)
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-e" . eval-last-sexp)
        ("C-c C-c" . eval-defun)
        ("C-c C-b" . eval-buffer)
        ("C-c C-r" . eval-region)))


(use-package eros
  :hook
  (emacs-lisp-mode-hook . eros-mode))


(use-package macrostep
  :bind
  (:map emacs-lisp-mode-map
        ("C-c <return> e" . macrostep-expand)))


;;;; docker

(use-package docker-compose-mode
  :mode
  ((rx (* nonl) "docker-compose"
       (* nonl) "y" (? "a") "ml") . docker-compose-mode))


(use-package docker
  :bind
  (:map ctl-x-map
        ("D" . docker)))


;;;; clojure

(use-package cider-mode
  :custom
  (cider-font-lock-dynamically nil)
  (cider-font-lock-reader-conditionals nil)
  (cider-prompt-for-symbol nil)
  (cider-special-mode-truncate-lines nil)
  (cider-eldoc-display-context-dependent-info t)
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-repl-display-in-current-window t)
  (cider-repl-buffer-size-limit 600)
  (nrepl-hide-special-buffers t)
  (cider-use-overlays t)
  :bind
  ((:map cider-mode-map
         ("C-c C-b" . cider-eval-buffer)
         ("C-c C-k" . cider-interrupt))
   (:map cider-repl-mode-map
         ("C-c C-b" . nil)
         ("C-c C-k" . cider-interrupt)))

  :init
  (advice-add 'cider-repl--insert-banner :override #'ignore))


(use-package flymake-kondor
  :hook
  (clojure-mode-hook . flymake-kondor-setup)
  (clojurec-mode-hook . flymake-kondor-setup)
  (clojurescript-mode-hook. flymake-kondor-setup))


;;;; scheme (primary guile)

(use-package geiser-mode
  :custom
  (geiser-active-implementations '(guile))
  :bind
  (:map geiser-mode-map
        ("C-c C-e" . geiser-eval-last-sexp)))


(use-package geiser-eros
  :hook
  (geiser-mode-hook . geiser-eros-mode))


;;;; org

(use-package org
  :defer t
  :custom
  (org-startup-indented t)
  (org-confirm-babel-evaluate nil)
  (org-babel-load-languages '((emacs-lisp . t)
                              (sql . t)
                              (shell . t))))


;;; emacs-integrations
;;;; irc

(use-package erc
  :commands erc-tls
  :custom
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-fill-static-center 16)
  (erc-fill-function #'erc-fill-static)
  (erc-interpret-mirc-color t)
  (erc-button-buttonize-nicks nil)
  (erc-insert-timestamp-function #'erc-insert-timestamp-left)
  (erc-autojoin-timing 'ident)
  (erc-autojoin-channels-alist '(("libera.chat" "#guix" "#emacs"
                                  "#guile" "#sr.ht" "#clojure")))
  (erc-autoaway-idle-method 'emacs)
  (erc-autoaway-idle-seconds 600)
  (erc-default-server "irc.libera.chat")
  (erc-default-port 6697)
  (erc-default-nicks '("kreved"))
  :config
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'scrolltobottom)
  (add-to-list 'erc-modules 'smiley)
  (add-to-list 'erc-modules 'autoaway)
  (erc-update-modules))


(use-package erc-hl-nicks
  :after erc)


(use-package erc-image
  :after erc)


;;;; pdf

(use-package pdf-view
  :mode
  ("\\.pdf\\'" . pdf-view-mode)
  :custom
  (pdf-view-display-size 'fit-height))


;;;; epub

(use-package nov
  :mode
  ("\\.epub\\'" . nov-mode))


;;;; git

(use-package magit
  :commands magit-status magit-project-status
  :custom
  (magit-save-repository-buffers nil)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :bind
  ([remap project-vc-dir] . magit-project-status))


(use-package evil-collection-magit
  :hook
  (magit-mode-hook . evil-collection-magit-setup)
  :custom
  (evil-collection-magit-want-horizontal-movement t)
  (evil-collection-magit-use-y-for-yank t))


;;;; ripgrep

(use-package rg
  :commands rg rg-project
  :bind
  (([remap project-find-regexp] . rg-project)
   :map search-map
   ("g" . rg)))


;;;; restclient

(use-package restclient
  :mode
  ("\\.http\\'" . restclient-mode))

;;;; kubernetes

(use-package kubel
  :commands kubel)

(use-package kubel-evil
  :hook
  (kubel-mode-hook . kubel-evil-mode))
