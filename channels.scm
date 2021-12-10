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
        (url "https://git.sr.ht/~krevedkokun/guix-channel")
	(commit "28ed6bf9112bf9a2107f123dc4e66f8749ef737f"))
       (channel
	(name 'graves)
	(url "file:///home/graves/.local/src/guix-channel.git")
	(commit "0c59ff5e8d53118060f6efb7dc697e1378f8f150"))
       (channel
        (name 'rde)
        (url "https://git.sr.ht/~abcdw/rde")
        (introduction
         (make-channel-introduction
          "257cebd587b66e4d865b3537a9a88cccd7107c95"
          (openpgp-fingerprint
           "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
       (channel
        (name 'manifesto)
        (url "file:///home/graves/docs/activism/pre/admin_dev/site_pre/manifesto/deploy/guix-channel")
        (commit "f483ab6b610b703bebf081103968990ed711ed55"))
       ;; (channel
       ;;  (name 'guix)
       ;;  (url "file:///home/graves/.local/src/guix")
       ;;  (commit "b8abc6c88a3d2a52d4f6292c9f7769d35968a0dc")
       ;;  (introduction
       ;;   (make-channel-introduction
       ;;    "9edb3f66fd807b096b48283debdcddccfea34bad"
       ;;    (openpgp-fingerprint
       ;;     "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
       %default-channels)
