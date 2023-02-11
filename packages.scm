(define-module (packages)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (gnu packages freedesktop)
  #:use-module (guix utils))

(define-public rofi-power-menu
  (package
    (name "rofi-power-menu")
    (version "3.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://raw.githubusercontent.com/jluttine/"
             name "/" version "/" name ))
       (sha256
        (base32 "0l7cckh9mn1yxd6ss3l89cks2zm6iwbmka95iivw10wgdr9yvid2"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'from-systemd-to-elogind
            (lambda _
              (substitute* "rofi-power-menu"
                (("loginctl")
                 #$(file-append elogind "/bin/loginctl"))
                (("systemctl")
                 #$(file-append elogind "/bin/loginctl")))
              (chmod "rofi-power-menu" #o755))))))
    (inputs (list elogind))
    (home-page "https://github.com/jluttine/rofi-power-menu")
    (synopsis "Basic power menu for rofi")
    (description "\
Rofi Power Menu provides a mode for offering basic power menu operations such
as shutting down, logging out, rebooting and suspending.  By default, it shows
all choices and asks for confirmation for irreversible actions.  The choices,
their order and whether they require confirmation, can be all configured with
command-line options.  It also shows symbols by default, but this requires a
monospace font with good support for symbols, so it can be disabled with
@code{--no-symbols}.

In contrast to other similar solutions, the power menu is implemented as a
rofi mode, not as a stand-alone executable that launches rofi by itself.  This
makes it possible to combine the script with the full power of how rofi can
use modi.  For instance, you can have multiple modi available (@code{-modi})
or combine multiple modi in one mode (-combi-modi), pass your own themes
(@code{-theme}) and configurations as CLI flags (e.g., @code{-fullscreen},
@code{-sidebar-mode}, @code{-matching fuzzy}, @code{-location}).")
    (license license:expat)))

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
