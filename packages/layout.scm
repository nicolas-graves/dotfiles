(define-module (packages layout)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download))

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
