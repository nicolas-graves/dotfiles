(define-module (packages xdisorg)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public rofi-power-menu-mode
  (package
    (name "rofi-power-menu-mode")
    (version "3.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jluttine/rofi-power-menu")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yrnjihjs8cl331rmipr3xih503yh0ir60mwsxwh976j2pn3qiq6"))))
    (build-system copy-build-system)
    (home-page "https://github.com/jluttine/rofi-power-menu")
    (synopsis "Basic power menu for rofi")
    (description "\
Rofi Power Menu provides a mode for offering basic power menu operations such
as shutting down, logging out, rebooting and suspending.  By default, it shows
all choices and asks for confirmation for irreversible actions.  The choices,
their order and whether they require confirmation, can be all configured with
command-line options.  It also shows symbols by default, but this requires a
monospace font with good support for symbols, so it can be disabled with
--no-symbols.

In contrast to other similar solutions, the power menu is implemented as a
rofi mode, not as a stand-alone executable that launches rofi by itself.  This
makes it possible to combine the script with the full power of how rofi can
use modi.  For instance, you can have multiple modi available (@code{-modi})
or combine multiple modi in one mode (-combi-modi), pass your own themes
(@code{-theme}) and configurations as CLI flags (e.g., @code{-fullscreen},
@code{-sidebar-mode}, @code{-matching fuzzy}, @code{-location}).")
    (license license:expat)))
