(define-module (packages bg)
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


