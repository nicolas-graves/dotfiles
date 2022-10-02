(define-module (packages xdisorg)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
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

(define-public rofi-switch-browser-tabs
  (let* ((commit "e1516cc2cb824fb9bdfe45680429e36d6e3789c5")
         (revision "0"))
    (package
      (name "rofi-switch-browser-tabs")
      (version (git-version "0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/nicolas-graves/rofi-switch-browser-tabs")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "15r3ajf60vgirrlapqf10xlklmwpj4k39sii9ji6djm16anpqwsm"))))
      (build-system copy-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-before 'install 'exe
             (lambda _ (chmod "switch-browser-tabs" #o555))))
         #:install-plan
         (list '("switch-browser-tabs" "bin/"))))
      (propagated-inputs (list wget jq))
      (home-page "https://github.com/nicolas-graves/rofi-switch-browser-tabs")
      (synopsis "Use rofi to switch between browser tabs")
      (description "This packages provides a simple script to switch between
browsers implementing the Chrome Debugging Protocol.")
      (license license:expat))))
