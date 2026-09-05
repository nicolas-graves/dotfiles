;; SPDX-License-Identifier: GPL-3.0-or-later
;; Explicit, independently updated lock for the rde-dev inferior.
;;
;; `guix time-machine -C'/`guix pull -C' load this file through a
;; sandboxed evaluator (%SAFE-CHANNEL-BINDINGS in guix/scripts/pull.scm)
;; that hands `channel' et al. directly into scope -- it does not
;; provide `use-modules' at all, so importing (guix channels) here is
;; both redundant and fatal ("unbound variable: use-modules").

(define (locked-channel name url commit)
  (channel (name name) (url url) (commit commit)))

;; Only channels an active Emacs feature in configuration.scm actually
;; needs, hard (no `or@' fallback): `growl-at' backs
;; feature-emacs-guile-ares-lsp/feature-emacs-mcp-server/the
;; tree-sitter-guix grammar. nonguix/odf-dsfr/snakemake-guix are only
;; ever referenced through `or@' (soft, degrades gracefully if absent)
;; and guix-science/guix-local/guix-stack/brainy/guix-ai-cloud aren't
;; referenced by configuration.scm at all -- none of those belong in
;; this inferior's lock.
;;
;; `guile-wayland' IS needed even though nothing here references it
;; directly: growl-at/.guix-channel declares it as a dependency (at the
;; canonical URL, branch master, no fixed commit), and that canonical
;; tip still uses wlroots-0.17/(gnu packages wm), both removed by the
;; guix pin above (renamed wlroots-0.19/0.20, module renamed to (gnu
;; packages window-management)). Listing it explicitly here, at your
;; own fork's fix commit, overrides growl-at's broken transitive pin --
;; same precedence rule that already lets the explicit `rde' entry
;; above override growl-at's own (also canonical, abcdw) dependency on
;; rde.
(list
 ;; The original pin (8d5a142) predated (gnu packages
 ;; window-management), which rde needs -- just too old, not a local-only
 ;; commit issue (the 43 commits this repo's local checkout carries above
 ;; origin/master are unrelated AI/agent-tooling packaging work, not
 ;; needed here). Bumped to a recent canonical origin/master commit.
 (locked-channel 'guix "https://git.guix.gnu.org/guix.git" "002b1a13c27a9fcaf52afe185820baed59d45507")
 (locked-channel 'rde "https://git.sr.ht/~ngraves/rde" "e72ec39bc3e6cbefcf7d1780d24104e0110d41f9")
 (locked-channel 'growl-at "https://git.sr.ht/~ngraves/growl-at/" "7b413c4b6a4004dcd48a21c1598ecf88ee8fdf5c")
 (locked-channel 'guile-wayland "https://github.com/nicolas-graves/guile-wayland-channel" "b4bf8773215ae8609c9ea072285a0a2d7676af75"))
