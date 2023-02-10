(define-module (files)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download))

(define-public programmer-beop
  (let ((commit "402305021b7fbf825aa4ea8381f4c5ae9a5ed81f"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/luxcem/programmer-beop")
            (commit commit)))
      (sha256
       (base32 "1y968pd3ynjngvwr1zkcwkvhkwblzs3isdak12zqzvgl1krdhqhv")))))

(define-public programmer-beop-image
  (origin
    (method url-fetch)
    (uri "https://luxcem.github.io/images/programmer_beop/prbeop.png")
    (sha256
     (base32 "10dzfwm75b7p3jk0i6hi3wkpjyfbj9jsx78zxzmm0cippphnpzw9"))))

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
    (home-page "https://alphacephei.com/vosk/models")
    (synopsis "US English model for vosk")
    (description "US English model for vosk")
    (license license:asl2.0)))
