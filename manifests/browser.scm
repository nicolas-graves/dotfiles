(use-modules
 (srfi srfi-26)
 (gnu packages)
 (guix profiles)
 (guix packages)
 (guix download)
 (guix build utils)
 (guix build-system copy)
 (gnu packages chromium)
 (gnu packages web-browsers)
 (gnu packages browser-extensions)
 (gnu packages compression)
 (gnu build chromium-extension)
 ((guix licenses) #:prefix license:))


(define bitwarden
  (package
    (name "bitwarden")
    (version "1.49.1")
    (home-page "https://github.com/bitwarden/browser")
    (source (origin
              (method url-fetch)
              (uri "https://github.com/bitwarden/browser/releases/download/v1.49.1/dist-chrome-3004.zip")
              (sha256
               (base32 "104igyr19r4appa3xj1i13w7b2p91v9dhyi83a0qvz15sb4r9v9c"))))
    (build-system copy-build-system)
    (native-inputs
     `(("unzip" ,unzip)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (invoke "unzip" source)
             (for-each (lambda (f) (chmod f (if (directory-exists? f) #o755 #o644)))
                       (find-files "." #:directories? #t)))))))
    (synopsis "")
    (description "")
    (license license:gpl3)))


(define bitwarden/chromium
  (make-chromium-extension bitwarden))


(packages->manifest
 (list nyxt
       ungoogled-chromium
       ;; ublock-origin/chromium
       ;; bitwarden/chromium
       ))

;; guix package --profile=$GUIX_EXTRA_PROFILES/browser/browser --manifest=$HOME/.config/guix/manifests/browser.scm
