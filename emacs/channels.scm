;; SPDX-License-Identifier: GPL-3.0-or-later
;; Explicit, independently updated lock for the rde-dev inferior.

(use-modules (guix channels))

(define (locked-channel name url commit)
  (channel (name name) (url url) (commit commit)))

(list
 (locked-channel 'guix "https://git.guix.gnu.org/guix.git" "8d5a142ed3b7557af407ad573d5c321b0a119ffc")
 (locked-channel 'rde "https://git.sr.ht/~abcdw/rde" "e72ec39bc3e6cbefcf7d1780d24104e0110d41f9")
 (locked-channel 'nonguix "https://gitlab.com/nonguix/nonguix.git" "3b66965566fe8c96edb5a41fd39a9e5a90ad9b61")
 (locked-channel 'odf-dsfr "https://github.com/codegouvfr/odf-dsfr" "af1b66927f2dc968549a978626150b5f2c1afd37")
 (locked-channel 'guix-stack "https://git.sr.ht/~ngraves/guix-stack" "47cced812b0e1529d80aef2b1a6c4dacf49fb492")
 (locked-channel 'guix-science "https://codeberg.org/guix-science/guix-science" "5375633ab9abd91de34c2c4ddb0c03bebcdcfd2d")
 (locked-channel 'guix-local "https://git.sr.ht/~ngraves/guix-local" "d521d049b04e07312ec9622f299eb6ce394e9868")
 (locked-channel 'brainy "https://codeberg.org/spritely/brainy" "d756bb159ce0bb42d34bb75ccbe8b85264923976")
 (locked-channel 'guile-wayland "https://github.com/guile-wayland/channel" "319ed15141dad6c9f273c84242df4d0d4b05bdcf")
 (locked-channel 'snakemake-guix "https://github.com/nicolas-graves/snakemake-guix" "0e829e486463fc71cb3809e79ad8f48d7d2658cf")
 (locked-channel 'guix-ai-cloud "https://codeberg.org/shegeley/guix-ai-cloud" "f1193389b6a327b9afcfe3034c0b9f4f19c2aaf4")
 (locked-channel 'growl-at "https://git.sr.ht/~ngraves/growl-at/" "99f6645a64699c8f39f58ab5b55ac4f3cc0f3cbf"))
