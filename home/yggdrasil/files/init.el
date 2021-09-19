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
  (use-short-answers t)
  (cursor-type 'bar))


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
                             (?e "Eshell" project-eshell))))


(use-package helpful
  :bind
  (([remap describe-function] . helpful-callable)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-key] . helpful-key)
   :map help-map
   ("d" . helpful-at-point)))

(use-package help
  :bind
  (:map ctl-x-map
        ("h" . help-command)))


(use-package which-key
  :defer t
  :custom
  (which-key-mode t))


(use-package uniquify
  :defer t
  :custom
  (uniquify-after-kill-buffer-p t)
  (uniquify-strip-common-suffix t)
  (uniquify-buffer-name-style 'post-forward))


(use-package simple
  :defer t
  :custom
  (column-number-indicator-zero-based nil)
  (column-number-mode t)
  (kill-whole-line t)
  :bind
  (("C-h" . delete-backward-char)))


(use-package paren
  :custom
  (show-paren-style 'parenthesis)
  (show-paren-when-point-inside-paren t)
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
        ("k" . kill-current-buffer)))


(use-package eldoc
  :defer t
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-mode t))


(use-package tab-bar
  :defer t
  :custom
  (tab-bar-show nil)
  (tab-bar-mode t))


(use-package indent
  :defer t
  :preface
  (provide 'indent)
  :custom
  (tab-always-indent 'complete))


(use-package delsel
  :defer t
  :custom
  (delete-selection-mode t))


(use-package elec-pair
  :defer t
  :custom
  (electric-pair-mode t))


(use-package bindings
  :preface
  (provide 'bindings)
  :bind-keymap
  ("C-c C-s" . search-map))


(use-package hippie-exp
  :bind
  (([remap dabbrev-expand] . hippie-expand)))

;;;; editing

(use-package paredit
  :commands paredit-mode
  :preface
  (defun kreved--enable-paredit ()
    (when (bound-and-true-p electric-pair-mode)
      (electric-pair-mode 0))
    (paredit-mode t))
  :hook
  (lisp-data-mode-hook . kreved--enable-paredit)
  (clojure-mode-hook . kreved--enable-paredit)
  (scheme-mode-hook . kreved--enable-paredit)
  (emacs-lisp-mode-hook . kreved--enable-paredit)
  :bind
  (:map paredit-mode-map
        ("M-[" . paredit-wrap-square)
        ("M-]" . paredit-wrap-curly)))


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
  :custom
  (modus-themes-syntax '(yellow-comments alt-syntax))
  (modus-themes-diffs 'fg-only-deuteranopia)
  (modus-themes-completions nil)
  (modus-themes-mode-line '(borderless))
  (modus-themes-region '(no-extend bg-only accented))
  (modus-themes-org-blocks 'tinted-background)
  (modus-themes-variable-pitch-headings t)
  (modus-themes-paren-match '(intense))
  (modus-themes-hl-line '(intense))
  (modus-themes-slanted-constructs t)
  (modus-themes-lang-checkers '(background intense))
  :init
  (load-theme 'modus-operandi t))


(use-package minions
  :defer t
  :custom
  (minions-direct '(flymake-mode))
  (minions-mode t))


(use-package frame
  :defer t
  :custom
  (blink-cursor-mode nil))


(use-package mini-frame
  :defer t
  :custom-face
  (child-frame-border ((t (:background "gray"))))
  :custom
  (mini-frame-show-parameters
   '((top . 0.85)
     (width . 0.75)
     (left . 0.5)
     (height . 10)
     (child-frame-border-width . 2)))
  (mini-frame-detach-on-hide nil)
  (mini-frame-color-shift-step 0)
  (mini-frame-resize 'not-set)
  (mini-frame-handle-completions nil)
  (mini-frame-ignore-functions
   '(y-or-n-p yes-or-no-p hack-local-variables-confirm))
  (mini-frame-mode t))


;;;; window management

(use-package window
  :defer t
  :custom
  (even-window-sizes nil)
  (display-buffer-alist
   `((,(rx "*" (| "H" "h") "elp" (? "ful" (* nonl)) "*")
      (display-buffer-in-side-window)
      (window-width . 0.5)
      (side . right)
      (slot . 0)
      (window-parameters (mode-line-format . none)))
     (,(rx "*info*")
      (display-buffer-in-side-window)
      (window-width . 0.5)
      (side . right)
      (slot . 0)
      (window-parameters (mode-line-format . none)))
     (,(rx "*cider-" (| "doc" "error" "test-report") "*")
      (display-buffer-in-side-window)
      (window-height . 0.4)
      (side . bottom)
      (slot . 0)
      (window-parameters (mode-line-format . none)))
     (,(rx "*cider-result*")
      (display-buffer-in-side-window)
      (window-height . 0.4)
      (side . bottom)
      (slot . 1)
      (window-parameters (mode-line-format . none)))
     (,(rx "*compilation*")
      (display-buffer-in-side-window)
      (window-height . 0.4)
      (side . bottom)
      (slot . -1)
      (window-parameters (mode-line-format . none)))
     (,(rx "*" (| "rg" "Occur") "*")
      (display-buffer-in-side-window)
      (window-height . 0.4)
      (side . bottom)
      (slot . 1)
      (window-parameters (mode-line-format . none))))))


;;;; completion

(use-package icomplete
  :demand t
  :preface
  (defun kreved--icomplete-setup ()
    (setq-local max-mini-window-height 10
                icomplete-tidy-shadowed-file-names t
                icomplete-show-matches-on-no-input t
                icomplete-hide-common-prefix nil
                icomplete-scroll nil
                completion-styles '(partial-completion orderless)
                completion-ignore-case t
                read-buffer-completion-ignore-case t
                read-file-name-completion-ignore-case t))
  :custom
  (completion-auto-help nil)
  (enable-recursive-minibuffers t)
  (resize-mini-windows 'grow-only)
  (icomplete-delay-completions-threshold 0)
  (icomplete-max-delay-chars 0)
  (icomplete-compute-delay 0)
  (completion-cycle-threshold 1)
  (minibuffer-depth-indicate-mode t)
  (minibuffer-electric-default-mode t)
  (icomplete-mode t)
  (icomplete-vertical-mode t)
  :hook
  (icomplete-minibuffer-setup-hook . kreved--icomplete-setup)
  :bind
  (:map minibuffer-local-map
        ("C-w" . backward-kill-word)
        :map icomplete-minibuffer-map
        ("C-k" . icomplete-fido-kill)
        ("C-d" . icomplete-fido-delete-char)
        ("C-m" . icomplete-fido-ret)
        ("<return>" . icomplete-fido-ret)
        ("C-h" . icomplete-fido-backward-updir)
        ("<backspace>" . icomplete-fido-backward-updir)
        ("M-j" . icomplete-fido-exit)
        ("C-." . nil)
        ("C-," . nil)))


(use-package marginalia
  :disabled t
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
    (when (string-suffix-p "|" pat)
      `(orderless-strict-leading-initialism . ,(drop-last pat))))

  (defun orderless-initialism-dispatcher (pat _index _total)
    (when (string-suffix-p "\\" pat)
      `(orderless-initialism . ,(drop-last pat))))

  :custom
  (orderless-matching-styles '(orderless-flex))
  (orderless-style-dispatchers '(orderless-literal-dispatcher
                                 orderless-sli-dispatcher
                                 orderless-initialism-dispatcher))
  :config
  (setq completion-category-defaults nil))


(use-package consult
  :bind
  (([remap apropos-command] . consult-apropos)
   ;; ([remap switch-to-buffer] . consult-buffer)
   ;; ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   :map search-map
   ("l" . consult-line)
   :map project-prefix-map
   ("i" . consult-project-imenu)
   :map goto-map
   ("M-i" . consult-imenu)
   ("M-o" . consult-outline)
   ("M-e" . consult-flymake)
   :map help-map
   ("M" . consult-man)))


(use-package embark
  :bind
  (("C-." . embark-act)))


(use-package embark-consult
  :after (embark consult))


;;;; search and movements

(use-package imenu
  :custom
  (imenu-auto-rescan t)
  (imenu-auto-rescan-maxout 60000)
  (imenu-use-popup-menu nil)
  (imenu-eager-completion-buffer t)
  :hook
  (imenu-after-jump-hook . recenter))


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


(use-package recentf
  :defer t
  :custom
  (recentf-mode t))


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
  (cider-save-file-on-load nil)
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


;;;; C

(use-package cc-mode
  :defer t)


;;;; Common Lisp

(use-package sly
  :bind
  (:map sly-mode-map
        ("C-c C-e" . sly-eval-last-expression)
        ("C-c C-c" . sly-eval-defun)
        ("C-c C-b" . sly-eval-buffer)))


;;; emacs-integrations
;;;; irc

(use-package erc
  :commands erc-tls erc-update-modules
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
  ("\\.epub\\'" . nov-mode)
  :custom
  (nov-text-width 80))


;;;; git

(use-package magit
  :commands magit-status
  :custom
  (magit-save-repository-buffers nil)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))


(use-package magit
  :after project
  :bind
  ([remap project-vc-dir] . magit-project-status)
  :init
  (add-to-list 'project-switch-commands '(?v "Magit" magit-project-status)))


;;;; ripgrep

(use-package rg
  :commands rg rg-project
  :bind
  (:map search-map
        ("g" . rg)))


(use-package rg
  :after project
  :bind
  (([remap project-find-regexp] . rg-project))
  :init
  (add-to-list 'project-switch-commands '(?g "Find regexp" rg-project)))


;;;; restclient

(use-package restclient
  :mode
  ("\\.http\\'" . restclient-mode))


;;;; direnv

(use-package direnv
  :defer t
  :custom
  (direnv-mode t))


;;;; mail

(use-package notmuch)


;; (use-package notmuch-hello
;;   :custom
;;   (notmuch-hello-sections '(notmuch-hello-insert-recent-searches
;;                             notmuch-hello-insert-alltags)))


(use-package message
  :custom
  (sendmail-program "msmtp")
  (message-sendmail-f-is-evil t)
  (send-mail-function 'message-send-mail-with-sendmail)
  (message-sendmail-extra-arguments '("--read-envelope-from"))
  (message-kill-buffer-on-exit t))


;;;; lsp

(use-package eglot
  :hook
  (c-mode-hook . eglot-ensure))