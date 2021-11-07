<<<<<<< HEAD
(cons*
 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  (introduction
   (make-channel-introduction
    "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
    (openpgp-fingerprint
     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
 (channel
  (name 'personal)
  (url "https://git.sr.ht/~krevedkokun/guix-channel")
  (introduction
   (make-channel-introduction
    "8adfc051b27c8add369345bcf44299ff8fc16fb2"
    (openpgp-fingerprint
     "9946 5567 F17F F3EF D363  0034 8469 C699 F664 6AC6"))))
 (channel
  (name 'rde)
  (url "https://git.sr.ht/~abcdw/rde")
  (introduction
   (make-channel-introduction
    "257cebd587b66e4d865b3537a9a88cccd7107c95"
    (openpgp-fingerprint
     "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
 %default-channels)
=======
(cons* (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (introduction
         (make-channel-introduction
          "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
          (openpgp-fingerprint
           "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
       (channel
        (name 'kreved)
        (url "https://git.sr.ht/~krevedkokun/guix-channel"))
       (channel
	 (name 'graves)
	 (url "file:///home/graves/.local/src/guix-channel.git")
	 (commit "a5675462077e856fe8ef5147985a5c688d07a001"))
       (channel
        (name 'rde)
        (url "https://git.sr.ht/~abcdw/rde")
        (introduction
         (make-channel-introduction
          "257cebd587b66e4d865b3537a9a88cccd7107c95"
          (openpgp-fingerprint
           "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
        (channel
         (name 'flat)
         (url "https://github.com/flatwhatson/guix-channel.git")
         (commit
          "7b8353ebbcf486e3344924d1cac0fa7ba47c371d")
         (introduction
          (make-channel-introduction
           "33f86a4b48205c0dc19d7c036c85393f0766f806"
           (openpgp-fingerprint
            "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))
       %default-channels)

>>>>>>> 6bcf47d (Installing last pgtk emacs.)
