;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2026 Nicolas Graves <ngraves@ngraves.fr>

;; Dedicated, slower channel lock for the Emacs 31 development root
;; (plans/rde-emacs/03-emacs-31-development-root.md). Resolves Emacs and all
;; Elisp/native editor dependencies independently of the main root's
;; ../channels.scm. Only explicit non-Elisp tools may come from the main
;; root instead of being pinned here.
;;
;; `guix rde emacs lock' is the only command that updates this file's pins
;; and the emacs/source submodule pointer, and only from a clean validated
;; snapshot; it never commits Git changes itself.

(use-modules (guix channels))

(list)
