(define-module (files)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download))

(define-public bg
  (package
    (name "bg")
    (version "0")
    (source
     (origin
       (method url-fetch)
       (uri "https://pour-un-reveil-ecologique.org/media/images/fond_pre.original.jpg")
       (file-name "fond_pre.jpg")
       (sha256
        (base32 "03rn4fw9j31s7hl635q872hzxj4bj5m9hkjd4iqzl8z4lk0n9iiy"))))
    (build-system copy-build-system)
    (home-page "https://pour-un-reveil-ecologique.org/")
    (synopsis "My background image")
    (description "My background image")
    (license license:non-copyleft)))

(define-public bg-lock
  (package
    (name "bg-lock")
    (version "0")
    (source
     (origin
       (method url-fetch)
       (uri "https://pour-un-reveil-ecologique.org/media/images/fond_lock_pre.original.jpg")
       (file-name "fond_lock_pre.jpg")
       (sha256
        (base32 "1cyvaj0yvy6zvzy9yf1z6i629rwjcq3dni01phb599sp4n2cpa8g"))))
    (build-system copy-build-system)
    (home-page "https://pour-un-reveil-ecologique.org/")
    (synopsis "My background image for lockscreen")
    (description "My background image for lockscreen")
    (license license:non-copyleft)))

(define-public base16-waybar
  (let* ((commit "d2f943b1abb9c9f295e4c6760b7bdfc2125171d2")
         (revision "0"))
    (package
      (name "base16-waybar")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mnussbaum/base16-waybar")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1kc4w98rbdb3lyqrf8lzc1xsgn1zxqlzwxry6fiz89ygn84gh5ks"))))
      (build-system copy-build-system)
      (home-page "https://github.com/mnussbaum/base16-waybar")
      (synopsis "Base16 CSS for waybar")
      (description "Base16 CSS color and templates for waybar.")
      (license license:expat))))

(define-public programmer-beop
  (let* ((commit "402305021b7fbf825aa4ea8381f4c5ae9a5ed81f")
         (revision "0"))
    (package
      (name "programmer-beop")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/luxcem/programmer-beop")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1y968pd3ynjngvwr1zkcwkvhkwblzs3isdak12zqzvgl1krdhqhv"))))
      (build-system copy-build-system)
      (home-page "https://xn--nxa.luxcem.fr/Programmer-beop/")
      (synopsis "Disposition de clavier ergonomique basé sur Bépo")
      (description "Disposition de clavier ergonomique basé sur Bépo")
      (license license:gpl3))))

(define-public programmer-beop-image
  (package
    (name "programmer-beop-image")
    (version "0")
    (source
     (origin
       (method url-fetch)
       (uri "https://luxcem.github.io/images/programmer_beop/prbeop.png")
       (file-name "prbeop.png")
       (sha256
        (base32 "10dzfwm75b7p3jk0i6hi3wkpjyfbj9jsx78zxzmm0cippphnpzw9"))))
    (build-system copy-build-system)
    (home-page "https://xn--nxa.luxcem.fr/Programmer-beop/")
    (synopsis "Disposition de clavier ergonomique basé sur Bépo")
    (description "Disposition de clavier ergonomique basé sur Bépo -
layout image (not saved)")
    (license license:gpl3)))

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
