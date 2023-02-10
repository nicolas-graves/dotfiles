#!/run/current-system/profile/bin/guile -s
!#

(use-modules
 (guix gexp)
 (ice-9 match)
 (ice-9 pretty-print))

(define* (channel-content
          #:key
          (freeze? #f)
          (urls
           '((nonguix . "https://gitlab.com/nonguix/nonguix.git")
             (rde     . "https://git.sr.ht/~abcdw/rde")
             (guix    . "https://git.savannah.gnu.org/git/guix.git")))
          (freeze-commits
           '((nonguix . "1aecd24155019cc524bca1c868729102c8b23f24")
             (rde     . "101313a691f074dcb34e9cbd4f13664df02f4ac7")
             (guix    . "688c3ef28220979e79ffd061c762bda84a663534"))))
  "This function generates then content of the channels file, with
optional commit pinning."
  `(list
    (channel
     (name 'nonguix)
     (url ,(cdr (assoc 'nonguix urls)))
     ,(if freeze? `(commit ,(cdr (assoc 'nonguix freeze-commits)))
          `(branch "master"))
     (introduction
      (make-channel-introduction
       "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
       (openpgp-fingerprint
        "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
    (channel
     (name 'rde)
     (url ,(cdr (assoc 'rde urls)))
     ,(if freeze? `(commit ,(cdr (assoc 'rde freeze-commits)))
          `(branch "master"))
     (introduction
      (make-channel-introduction
       "257cebd587b66e4d865b3537a9a88cccd7107c95"
       (openpgp-fingerprint
        "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
    (channel
     (name 'guix)
     (url ,(cdr (assoc 'guix urls)))
     ,(if freeze? `(commit ,(cdr (assoc 'guix freeze-commits)))
          `(branch "master"))
     (introduction
      (make-channel-introduction
       "9edb3f66fd807b096b48283debdcddccfea34bad"
       (openpgp-fingerprint
        "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))

(pretty-print
 (channel-content
  #:freeze? #f
  #:freeze-commits
  '((guix    . "5f8c11d48e4949aa77d7aaa1e7e25568bd8dfa97")
    (nonguix . "e026dba1dad924aa09da8a28caa343a8ace3f6c7")
    (rde     . "74a3fb8378e86603bb0f70b260cbf46286693392"))
  #:urls
  '((guix    . "/home/graves/spheres/info/guix")
    (nonguix . "/home/graves/spheres/info/nonguix")
    (rde     . "/home/graves/spheres/info/rde"))))
