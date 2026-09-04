;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2026 Nicolas Graves <ngraves@ngraves.fr>

;; Dedicated, slower channel lock for the Emacs 31 development root
;; (plans/rde-emacs/03-emacs-31-development-root.md). Resolves all
;; Elisp/native editor dependencies independently of the main root's
;; ../channels.scm. Only explicit non-Elisp tools may come from the main
;; root instead of being pinned here.
;;
;; Mirrors ../channels.scm: every subdirectory of emacs/channels/ is a git
;; submodule, and (guix-submodule channels) turns the directory into a list
;; of <channel> pinned at each submodule's checked-out commit.
;;
;; `guix rde emacs lock' is the only command that updates this file's pins
;; and the emacs/source submodule pointer, and only from a clean validated
;; snapshot; it never commits Git changes itself.

(use-modules (guix channels))

(define %cwd (dirname (current-filename)))

(catch #t
  (lambda ()
    (use-modules (guix-submodule channels))
    (submodules-dir->channels
     (string-append %cwd "/channels") #:use-local-urls? #t))
  (lambda (key . args)
    (display "(guix-submodule channels) not found. Falling back...\n")
    (load (string-append
           %cwd "/../channels/guix-submodule/src/guix-submodule/channels.scm"))
    (use-modules (guix-submodule channels))
    (submodules-dir->channels
     (string-append %cwd "/channels") #:use-local-urls? #t)))
