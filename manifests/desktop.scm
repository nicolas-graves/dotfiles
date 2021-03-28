(use-modules (guix utils)
             (gnu packages autotools)
             (gnu packages pulseaudio)
             (guix packages)
             (guix git-download)
             (gnu packages)
             (guix profiles))

(define pulseaudio-git
  (package/inherit pulseaudio
    (version "45d896f")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/pulseaudio/pulseaudio")
                    (commit "45d896f8eb385a9719fdea3eacc2e42bc2281834")))
              (sha256
               (base32 "145iiam91ccprl4nr7m5ggh2chqif57nrs2c7d5alvsf5l9hzn1c"))))
    (native-inputs `(("autoconf" ,autoconf-wrapper)
                     ("automake" ,automake)
                     ("libtool" ,libtool)
                     ,@(package-native-inputs pulseaudio)))
    (arguments
     (substitute-keyword-arguments (package-arguments pulseaudio)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-before 'bootstrap 'prepare-for-bootstrap
             (lambda _
               (substitute* "git-version-gen"
                 (("/bin/sh") (which "sh")))
               (with-output-to-file ".tarball-version"
                 (lambda _ (format #t "45d896f")))))))))))

(packages->manifest
 `(,pulseaudio-git
   ,@(map specification->package
          '("xclip"
            "dmenu"
            "maim"
            "dunst"
            "feh"
            "sxiv"
            "picom"
            "alacritty"
            "xrandr"
            "pavucontrol-qt"
            "telegram-desktop"
            "bluez"
            "alsa-utils"
            ))))

;; guix package --profile=$GUIX_EXTRA_PROFILES/desktop/desktop --manifest=$HOME/.config/guix/manifests/desktop.scm
