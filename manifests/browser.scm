(use-modules
 (gnu packages)
 (guix profiles)
 (guix packages)
 (guix utils)
 (gnu packages chromium)
 (gnu packages web-browsers)
 (nongnu packages mozilla))


(packages->manifest
 (list
  nyxt
  firefox
  ungoogled-chromium
  ))

;; guix package --profile=$GUIX_EXTRA_PROFILES/browser/browser --manifest=$HOME/.config/guix/manifests/browser.scm
