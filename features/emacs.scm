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

  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix transformations)

  #:use-module (packages emacs)

  #:export (feature-emacs-evil
            feature-emacs-saving
            feature-emacs-elfeed
            feature-emacs-org-protocol
            feature-emacs-lispy
            feature-emacs-flycheck
            feature-emacs-web-mode
            feature-emacs-guix-development
            feature-emacs-dired-hacks
            feature-emacs-org-babel
            feature-emacs-org-latex
            feature-emacs-python
            feature-emacs-my-org-agenda
            feature-emacs-citar
            feature-emacs-eval-in-repl
            feature-emacs-origami-el))


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
         (require 'undo-fu)
         (require 'evil-collection))

        (setq evil-want-keybinding nil)

        (evil-mode 1)
        (setq evil-want-integration t)
        (setq evil-want-C-u-scroll t)
        (setq evil-want-C-i-jump nil)
        (setq evil-respect-visual-line-mode t)
        (setq evil-undo-system 'undo-fu)

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

         (evil-collection-init)
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
        (super-save-mode 1)
        (with-eval-after-load
         'super-save
         (setq super-save-auto-save-when-idle t))

        (global-undo-fu-session-mode)

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

(define*
  (feature-emacs-my-org-agenda
   #:key
   (org-agenda-files 'nil)
   (org-agenda-custom-commands
    ``((,(kbd "C-d") "Agenda for the day"
        ((agenda
          ""
          ((org-agenda-span 1)
           (org-agenda-scheduled-leaders '("" "Sched.%2dx: "))
           (org-agenda-block-separator nil)
           (org-agenda-entry-types '(:scheduled :timestamp :sexp))
           (org-scheduled-past-days 0)
           ;; We don't need the `org-agenda-date-today'
           ;; highlight because that only has a practical
           ;; utility in multi-day views.
           (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
           ;; (org-agenda-skip-function
           ;;  '(org-agenda-skip-entry-if 'todo '("NEXT")))
           (org-agenda-format-date "%A %-e %B %Y")
           (org-agenda-overriding-header "\nAgenda for the day\n")))
         (todo
          "NEXT"
          ((org-agenda-block-separator nil)
           (org-agenda-overriding-header "\nCurrent Tasks\n")))))
       (,(kbd "C-o") "Overview"
        ;; TODO: Add A priority to the top.
        ((agenda
          ""
          ((org-agenda-time-grid nil)
           (org-agenda-start-on-weekday nil)
           (org-agenda-start-day "+1d")
           (org-agenda-span 14)
           (org-agenda-show-all-dates nil)
           (org-agenda-time-grid nil)
           (org-agenda-show-future-repeats nil)
           (org-agenda-block-separator nil)
           (org-agenda-entry-types '(:deadline))
           (org-agenda-skip-function '(org-agenda-skip-entry-if 'done))
           (org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n")))
         (agenda
          "*"
          ((org-agenda-block-separator nil)
           (org-agenda-span 14)
           (org-agenda-show-future-repeats nil)
           (org-agenda-skip-deadline-prewarning-if-scheduled t)
           (org-agenda-overriding-header "\nAgenda\n")))
         (alltodo
          ""
          ((org-agenda-block-separator nil)
           (org-agenda-skip-function '(or (org-agenda-skip-if nil '(scheduled))))
           (org-agenda-overriding-header "\nBacklog\n"))))))))
  "Configure org-agenda for GNU Emacs."
  (define emacs-f-name 'my-org-agenda)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (require-value 'emacs-org config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
         (require 'org-agenda))
        (define-key global-map (kbd "C-x C-a") 'org-agenda)
        (with-eval-after-load
         'org-agenda
         ;; Impressive agenda examples
         ;; https://github.com/fniessen/emacs-leuven/blob/master/org-leuven-agenda-views.txt
         ;; Clean agenda view
         ;; https://gist.github.com/rougier/ddb84c16c28f7cd75e27e50d4c3c43da
         ;; https://d12frosted.io/posts/2020-06-23-task-management-with-roam-vol1.html
         (setq org-agenda-custom-commands ,org-agenda-custom-commands)
         (setq org-agenda-tags-column
               ;; TODO: Name this value better
               ,(- (get-value 'olivetti-body-width config 85)))
         (setq org-agenda-window-setup 'current-window)
         (setq org-agenda-files ',org-agenda-files))

        ;; Make done tasks show up in the agenda log
        (setq org-log-done 'time))
      #:summary "\
Preconfigured agenda views"
      #:commentary "\
Reasonable keybindings, preconfigured agenda views and integration with
olivetti package."
      #:keywords '(convenience))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

;; ((eval-when-compile (require 'org-roam-ui))
;;                  (with-eval-after-load
;;                   'org-roam-ui
;;                   (setq org-roam-ui-sync-theme t
;;                         org-roam-ui-follow t
;;                         org-roam-ui-update-on-save t
;;                         org-roam-ui-open-on-start t)))

(define* (feature-emacs-citar
          #:key
          (emacs-citar emacs-citar)
          (citar-library-paths (list "~/resources/files/library"))
          (citar-notes-paths (list "~/resources"))
          (global-bibliography (list "~/resources/biblio.bib")))
  "Configure org-cite and citar for GNU Emacs."
  (ensure-pred list? citar-library-paths)
  (ensure-pred list? citar-notes-paths)
  (ensure-pred list? global-bibliography)

  (define emacs-f-name 'citar)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
         (require 'citar)
         (require 'oc-biblatex)
         (require 'oc-csl))

        (setq org-cite-global-bibliography (list ,@global-bibliography))
        (setq org-cite-insert-processor 'citar)
        (setq org-cite-follow-processor 'citar)
        (setq org-cite-activate-processor 'citar)
        (setq org-cite-export-processors
              '((latex biblatex)
                (t csl)))

        (setq citar-library-paths (list ,@citar-library-paths))
        (setq citar-notes-paths (list ,@citar-notes-paths))

        ,@(if (get-value 'emacs-completion config)
              `((citar-embark-mode 1))
              '())

        ,@(if (get-value 'emacs-org-roam config)
              `((citar-org-roam-mode 1))
              '())

        (setq citar-bibliography org-cite-global-bibliography)
        (defun rde-find-main-bibliography ()
          "Find and open main bibliography file."
          (interactive) (find-file ,(car global-bibliography)))
        (define-key global-map (kbd "C-c b") 'org-cite-insert)
        (define-key global-map (kbd "C-c n b") 'rde-find-main-bibliography))
      #:summary "\
Reference management with emacs and citar"
      #:commentary "\
Set org-cite processors and citar configuration, basic keybindings, reasonable
defaults."
      #:keywords
      '(convenience org-mode org-cite citar references roam knowledgebase)
      #:elisp-packages
      (list emacs-citar
            (if (get-value 'emacs-org-roam config) emacs-citar-org-roam)))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-citar)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-elfeed
          #:key
          (emacs-elfeed emacs-elfeed)
          (emacs-elfeed-org emacs-elfeed-org)
          (elfeed-org-files '())
          (capture-in-browser? #f))
  "Setup and configure Elfeed for Emacs."
  (ensure-pred list-of-strings? elfeed-org-files)
  (ensure-pred file-like? emacs-elfeed)
  (ensure-pred file-like? emacs-elfeed-org)
  (ensure-pred boolean? capture-in-browser?)

  (define emacs-f-name 'elfeed)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (when (get-value 'emacs config)
       (rde-elisp-configuration-service
        emacs-f-name
        config
        `((require 'configure-rde-keymaps)
          (define-key rde-app-map (kbd "e") 'elfeed)
          (eval-when-compile (require 'elfeed) (require 'elfeed-org))
          (setq rmh-elfeed-org-files ',elfeed-org-files)

          ,@(if capture-in-browser?
                `((with-eval-after-load
                   'org-capture
                   (add-to-list
                    'org-capture-templates
                    '("r" "rssadd" entry
                      (file+headline ,(car elfeed-org-files)
                                     "Untagged")
                      "*** %:annotation\n"
                      :immediate-finish t))))
                '())

          (with-eval-after-load
           'elfeed
           (elfeed-org)))
        #:summary "\
Elfeed Emacs interface"
        #:commentary "\
Keybinding in `rde-app-map', xdg entry for adding rss feed.
In this version, elfeed relies on an elfeed-org configuration.
capture-in-browser? needs to set
\"javascript:location.href='org-protocol://capture?%27 +new
URLSearchParams({template: %27r%27, url: window.location.href,title:
document.title, body: window.getSelection()});\" as a web bookmark."
        #:keywords '(convenience)
        #:elisp-packages
        (list emacs-elfeed emacs-elfeed-org
              (get-value 'emacs-configure-rde-keymaps config))))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-elfeed)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-org-protocol)
  "Setup and configure Org-Protocol for Emacs."

  (define emacs-f-name 'org-protocol)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (define emacs-cmd (get-value 'emacs-client config))

    (list
     (when (get-value 'emacs config)
       (rde-elisp-configuration-service
        emacs-f-name
        config
        `((require 'org-protocol))
        #:summary "\
Org Protocol Emacs"
        #:commentary "\
Adding xdg-mime-entry and loading org-protocol.
This integrates well with elfeed for now."
        #:keywords '(convenience)
        #:elisp-packages '()))

     (when emacs-cmd
       (simple-service
        'home-xdg-applications
        home-xdg-mime-applications-service-type
        (home-xdg-mime-applications-configuration
         ;; The imv entry is included in the package, but chromium was set as default.
         (default '((x-scheme-handler/org-protocol . emacsclient.desktop)))
         (desktop-entries
          (list
           (xdg-desktop-entry
            (file "emacsclient")
            (name "emacsclient")
            (config `((exec . ,(file-append emacs-cmd " %u"))
                      (icon . "emacs")))
            (type 'application)))))))))

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
                 'ob
                 (define-key org-mode-map (kbd "C-<return>") 'ober-eval-in-repl)
                 (define-key org-mode-map (kbd "M-<return>") 'ober-eval-block-in-repl)))
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

(define* (feature-emacs-org-latex
          #:key
          (export-source-code? #f))
  "Configure emacs for compiling latex files."

  (define emacs-f-name 'org-latex)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `(,@(if export-source-code?
              `((setq org-latex-listings 'minted)
                (setq org-latex-packages-alist '(("" "minted"))))
              '())
        (setq org-latex-pdf-process
              '("lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f")))
      #:elisp-packages (if export-source-code? (list python-pygments) '())
      #:summary "\
Configuration tweaks to be able to produce latex documents from org-mode."
      #:commentary "\
")))

  (feature
   (name f-name)
   (values `((,f-name . 'emacs-org-latex)))
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

(define* (feature-emacs-guix-development
          #:key
          (guix-load-path "~/src/guix")
          (other-guile-load-paths '())
          (snippets-path "~/.config/guix/snippets/*.eld"))
  "Configure emacs for guix development."
  ;; FIXME Both guix-load-path and other-guile-load-paths
  ;; need to be absolute without ~ to work properly.
  (ensure-pred string? guix-load-path)
  (ensure-pred string? snippets-path)
  (ensure-pred list? other-guile-load-paths)

  (define emacs-f-name 'guix-development)
  (define f-name (symbol-append 'emacs- emacs-f-name))

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
                 (add-to-list 'tempel-path ,snippets-path)))
                 ;; (add-to-list
                 ;;  'tempel-path
                 ;;  ,(file-append (package-source
                 ;;                 (channel-source->package
                 ;;                  (package-source (current-guix))
                 ;;                  #:commit "47c11772dfe840a536ed7ec438fe832878f51054"))
                 ;;                "/etc/snippets/tempel/*"))))
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