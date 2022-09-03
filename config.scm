
;;;; System


;;; Channels
(use-modules
 (ice-9 match)
 (ice-9 pretty-print))

(define* (dots-channel
          #:key
          (freeze? #f)
          (freeze-commits
           '((nonguix     . "674d04a5fbd8689ab5ff27271a656f711fc77c54")
             (rde         . "051e0f77aef5610d1e74745cf9e2303b034462c3")
             (guix        . "8f0d45ccac3f6cee69eba8de5e4ae5e5555f1a3d"))))
  `(channel
    (version 0)
    (url "/home/graves/spheres/info/dots")
    (dependencies
     (channel
      (name nonguix)
      (url "https://gitlab.com/nonguix/nonguix")
      ,(if freeze? `((commit ,(cdr (assoc 'nonguix freeze-commit))))
           `(branch "master"))
      (introduction
       (make-channel-introduction
        "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
        (openpgp-fingerprint
         "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
     (channel
      (name rde)
      (url "https://git.sr.ht/~abcdw/rde")
      ,(if freeze? `((commit ,(cdr (assoc 'rde freeze-commit))))
           `(branch "master"))
      (introduction
       (make-channel-introduction
        "257cebd587b66e4d865b3537a9a88cccd7107c95"
        (openpgp-fingerprint
         "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
     (channel
      (name guix)
      (url "https://git.savannah.gnu.org/git/guix.git")
      ,(if freeze? `((commit ,(cdr (assoc 'guix freeze-commit))))
           `(branch "master"))
      (introduction
       (make-channel-introduction
        "9edb3f66fd807b096b48283debdcddccfea34bad"
        (openpgp-fingerprint
         "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))))


;;; Dispatcher

(define (dispatcher)
  (let ((rde-target (getenv "RDE_TARGET")))
    (match rde-target
      ;; ("home" %he)
      ;; ("system" %os)
      ;; ("live-system" live-os)
      ;; ("live-install" live-usb)
      ("channel"
        (with-output-to-file ".guix-channel"
           (lambda () (pretty-print (dots-channel)))))
      ;; (_ %he)
      )))

;; (pretty-print-rde-config ixy-config)
;; (use-modules (gnu services)
;; 	     (gnu services base))
;; (display
;;  (filter (lambda (x)
;; 	   (eq? (service-kind x) console-font-service-type))
;; 	 (rde-config-system-services ixy-config)))

;; (use-modules (rde features))
;; ((@ (ice-9 pretty-print) pretty-print)
;;  (map feature-name (rde-config-features ixy-config)))

(dispatcher)
