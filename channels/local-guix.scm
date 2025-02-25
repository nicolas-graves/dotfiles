(use-modules (guix channels)
             (guix derivations)
             (guix profiles)
             (guix store)
             (guix-stack build channel)
             (guix-stack build local-build-system)
             (ice-9 ftw)
             (srfi srfi-1)
             (srfi srfi-26))

;; GNU Guix is phenomenal in terms of extensibility and software
;; reproducibility. Some recent blog articles summed up how to use
;; Guix for local package development, see:
;; https://guix.gnu.org/blog/2023/from-development-environments-to\
;; -continuous-integrationthe-ultimate-guide-to-software-development-with-guix
;; One drawback of local development with Guix is the inability to
;; reuse compiled binary files for rapid software development: Guix
;; systematically rebuilds the whole package using all build phases.
;; This makes developping / hacking on heavy packages quite tedious.
;; In the absence of a better alternative, this hack/script allows to
;; develop locally by creating an equivalent store output from a local
;; repository using build phases from Guix source.

;; This requires a guix with patch series 68315 v3 applied to be run.

;; Important : We need to go through the store and derivations, since
;; we want to get the phases from Guix source. However, the derivation
;; builder can only affect the store. Thus the code needs to be
;; executed by the user. (I've also tried wide directory permissions,
;; which aren't enough. Maybe there's a way to build this using the
;; build daemon with the --disable-chroot option. But starting a new
;; daemon for this seems overkill).

;; We separate phases that are only needed to be applied once and phases
;; that need to be repeated each time the source is modified.

(with-store store
  (run-with-store store
    (profile-derivation
     (local-channels->manifest
      (canonicalize-path (getcwd))
      (filter (lambda (file)
                (and (eq? (stat:type (lstat file)) 'directory)
                     (not (member file '("." ".." "guix-science-nonfree")))))
              (scandir ".")))
     #:hooks %channel-profile-hooks
     #:format-version 3)))
