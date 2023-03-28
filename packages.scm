;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2022,2023 Nicolas Graves <ngraves@ngraves.fr>

(define-module (packages)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (rde packages emacs-xyz))

(define-public emacs-eval-in-repl-geiser-latest
  (package
    (inherit emacs-eval-in-repl-geiser)
    (propagated-inputs
     (modify-inputs (package-propagated-inputs emacs-eval-in-repl-geiser)
                    (replace "emacs-geiser" emacs-geiser-latest)))))

(define-public vosk-model-small-fr
  (package
    (name "vosk-model-small-fr")
    (version "0.22")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append
             "https://alphacephei.com/vosk/models/" name "-" version ".zip"))
       (sha256
        (base32 "1b8jps3xgzm3d5fmwlh9glx9ym2yplvs8hwxk8x9psvpw6063gya"))))
    (build-system copy-build-system)
    (arguments '(#:substitutable? #f))
    (home-page "https://alphacephei.com/vosk/models")
    (synopsis "Small french model for vosk")
    (description "Small french model for vosk")
    (license license:asl2.0)))

(define-public vosk-model-fr
  (package
    (name "vosk-model-fr")
    (version "0.22")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append
             "https://alphacephei.com/vosk/models/" name "-" version ".zip"))
       (sha256
        (base32 "0ihy93n6m5v9q22ky2hs1yvavsck3l592ppgdkp9v7qvxbjk8v5j"))))
    (build-system copy-build-system)
    (arguments '(#:substitutable? #f))
    (home-page "https://alphacephei.com/vosk/models")
    (synopsis "French model for vosk")
    (description "French model for vosk")
    (license license:asl2.0)))

(define-public vosk-model-small-en-us
  (package
    (name "vosk-model-small-en-us")
    (version "0.15")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append
             "https://alphacephei.com/vosk/models/" name "-" version ".zip"))
       (sha256
        (base32 "1614jj01gx4zz5kq6fj2lclwp1m6swnk1js2isa9yi7bqi165wih"))))
    (build-system copy-build-system)
    (arguments '(#:substitutable? #f))
    (home-page "https://alphacephei.com/vosk/models")
    (synopsis "US English small model for vosk")
    (description "US English small model for vosk")
    (license license:asl2.0)))

(define-public vosk-model-en-us
  (package
    (name "vosk-model-en-us")
    (version "0.22")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append
             "https://alphacephei.com/vosk/models/" name "-" version ".zip"))
       (sha256
        (base32 "1xk9gg15ikv47qqbdhlnny9hhg1rmhv5q5qrsc5vp783pcgaiya7"))))
    (build-system copy-build-system)
    (arguments '(#:substitutable? #f))
    (home-page "https://alphacephei.com/vosk/models")
    (synopsis "US English model for vosk")
    (description "US English model for vosk")
    (license license:asl2.0)))
