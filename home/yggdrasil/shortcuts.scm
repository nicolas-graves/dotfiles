#! /run/current-system/profile/bin/guix repl --
!#

(use-modules (gnu home services))
(use-modules (gnu home-services-utils))
(use-modules (gnu home services shepherd))
(use-modules (gnu services configuration))

(use-modules (srfi srfi-1))
(use-modules (ice-9 match))

(use-modules (guix packages))
(use-modules (guix gexp))
(use-modules (guix diagnostics))
(use-modules (guix i18n))
(use-modules ((guix import utils) #:select (flatten)))
(use-modules (gnu home services))

;; The idea would be to have a
;; Maybe there's a need to produce different serializations depending on the final
;; output format ?

;;
;; Data structures
;;

;; Define a format with different options for having to see the file

;; Directories

(define dirs-shortcuts
  `((cac "${XDG_CACHE_HOME:-$HOME/.cache}")
    (cf "${XDG_CONFIG_HOME:-$HOME/.config}")
    (D "${XDG_DOWNLOAD_DIR:-$HOME/tels}")
    (d "${XDG_DOCUMENTS_DIR:-$HOME/docs}")
    (dt "${XDG_DATA_HOME:-$HOME/.local/share}")
    (rr "${XDG_DATA_HOME:-$HOME/.local/src}")
    (h "$HOME")
    (m "${XDG_MUSIC_DIR:-$HOME/music}")
    (mn "/mnt")
    (pp "${XDG_PICTURES_DIR:-$HOME/images}")
    (sc "$HOME/.local/bin")
    (src "$HOME/.local/src")
    (vv "${XDG_VIDEOS_DIR:-$HOME/videos}")
    (site "$HOME/docs/pre/admin_dev/site_pre/manifesto")
    (diary "$HOME/docs/vimwiki/diary")
    ))

;; Folders

(define (serialize-shortcut-config field-name val)
  (define (serialize-term term)
    (match term
      ((? symbol? e) (symbol->string e))
      ((? number? e) (format #f "~a" e))
      ((? string? e) (format #f "~s" e))
      (e e)))
  (define (serialize-item entry)
    (match entry
      ((? gexp? e) e)
      ((? list lst)
       #~(string-join '#$(map serialize-term lst)))))

  #~(string-append #$@(interpose (map serialize-item val) "\n" 'suffix)))

;; (define-configuration/no-serialization home-shortcut-configuration
;;   (config
;;    (dirs-shortcuts)
;;    "AList of pairs, each pair is a String and String or Gexp."))

`(gexp->file
   "dirs.shortcuts"
   (serialize-shortcut-config "dirs-shortcuts" dirs-shortcuts))
