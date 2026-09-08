;; SPDX-License-Identifier: GPL-3.0-or-later
;; Explicit rde-dev channel lock; updated only by `guix rde emacs lock'.

(list
 (channel (name 'guix) (url "https://git.guix.gnu.org/guix.git") (branch "master") (commit "cc99151c6a61e6f25534458cdffeb8d048928bcc") (introduction (make-channel-introduction "9edb3f66fd807b096b48283debdcddccfea34bad" (openpgp-fingerprint "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
 (channel (name 'rde) (url "https://git.sr.ht/~ngraves/rde") (branch "master") (commit "0a96a37f18a86bc50fa237f0c5d4586ac46e7386") (introduction (make-channel-introduction "257cebd587b66e4d865b3537a9a88cccd7107c95" (openpgp-fingerprint "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
 (channel (name 'growl-at) (url "https://git.sr.ht/~ngraves/growl-at/") (branch "master") (commit "d9594660d3ad05651fbb81f18eb1a913900fd1b2"))
 (channel (name 'guile-wayland) (url "https://github.com/nicolas-graves/guile-wayland-channel") (branch "master") (commit "b4bf8773215ae8609c9ea072285a0a2d7676af75"))
 (channel (name 'guix-science) (url "https://codeberg.org/guix-science/guix-science") (branch "master") (commit "6d52a6263e1afc31b10db15df0931e0e43bfdc6c"))
)
