(use-modules
 (gnu packages)
 (guix profiles)
 (guix packages)
 (guix utils)
 (gnu packages chromium)
 (nongnu packages mozilla))


;; fixes screen-sharing in google-meet
(define ungoogled-chromium-with-hangouts-services
  (package/inherit
   ungoogled-chromium
   (arguments
    (substitute-keyword-arguments
     (package-arguments ungoogled-chromium)
     ((#:configure-flags flags)
      `(cons "enable_hangout_services_extension=true" ,flags))))))

(packages->manifest
 (list
  firefox
  ungoogled-chromium-with-hangouts-services
  ))

;; guix package --profile=$GUIX_EXTRA_PROFILES/browser/browser --manifest=$HOME/.config/guix/manifests/browser.scm
