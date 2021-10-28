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
