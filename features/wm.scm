;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2022 Samuel Culpepper <samuel@samuelculpepper.com>
;;;
;;; This file is part of rde.
;;;
;;; rde is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; rde is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with rde.  If not, see <http://www.gnu.org/licenses/>.

;;; This file is copied from rde in order allow closer configuration to my defaults.
;;; It should be gone as soon as rde allows for flexible configuration of keybindings.

(define-module (features wm)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde features fontutils)
  #:use-module (gnu system)
  #:use-module (gnu system keyboard)
  #:use-module (rde packages)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages image)
  #:use-module (gnu packages web)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xdisorg)
  #:use-module (packages xdisorg)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages fonts)
  #:use-module (gnu services)
  #:use-module (gnu services xorg)
  #:use-module (gnu services shepherd)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home-services-utils)
  #:use-module (rde home services wm)
  #:use-module (gnu home-services shells)

  #:use-module (guix gexp)
  #:use-module (guix packages)

  #:use-module (srfi srfi-1)

  #:export (ng-feature-sway))


;;;
;;; Sway.
;;;

;; https://github.com/jjquin/dotfiles/tree/master/sway/.config/sway/config.d
;; https://nixos.wiki/wiki/Sway
;; https://github.com/swaywm/sway/wiki/Useful-add-ons-for-sway

(define (keyboard-layout-to-sway-config keyboard-layout)
  (let ((kb-options (string-join
		     (keyboard-layout-options keyboard-layout) ",")))
    `((input *
	     ((xkb_layout  ,(keyboard-layout-name keyboard-layout))
	      (xkb_variant ,(keyboard-layout-variant keyboard-layout))
	      (xkb_options ,kb-options))))))

(define* (ng-feature-sway
	  #:key
	  (extra-config '())
	  (sway sway)
          (foot foot)
          (bemenu bemenu)
          (xdg-desktop-portal xdg-desktop-portal)
          (xdg-desktop-portal-wlr xdg-desktop-portal-wlr)
          ;; Logo key. Use Mod1 for Alt.
          (sway-mod 'Mod4)
	  (add-keyboard-layout-to-config? #t)
          (xwayland? #f))
  "Setup and configure sway."
  (ensure-pred sway-config? extra-config)
  (ensure-pred boolean? add-keyboard-layout-to-config?)
  (ensure-pred boolean? xwayland?)
  (ensure-pred any-package? sway)
  (ensure-pred any-package? foot)
  (ensure-pred any-package? bemenu)
  (ensure-pred any-package? xdg-desktop-portal)
  (ensure-pred any-package? xdg-desktop-portal-wlr)

  (define (sway-home-services config)
    "Returns home services related to sway."
    (let* ((kb-layout      (get-value 'keyboard-layout config))
	   (layout-config  (if (and add-keyboard-layout-to-config? kb-layout)
			       (keyboard-layout-to-sway-config kb-layout)
			       '()))

           (lock-cmd
            (get-value 'default-screen-locker config "loginctl lock-session"))

           (default-terminal
             (get-value-eval 'default-terminal config
                             (file-append foot "/bin/foot")))
           (backup-terminal
             (get-value 'backup-terminal config
                        (file-append foot "/bin/foot")))
           (default-application-launcher
             (get-value 'default-application-launcher config
                        (file-append bemenu "/bin/bemenu-run -l 20 -p run:"))))
      (list
       (service
	home-sway-service-type
	(home-sway-configuration
	 (package sway)
         (config
          `((xwayland ,(if xwayland? 'enable 'disable))
            (,#~"")
            ,@layout-config

            (,#~"\n\n# General settings:")
            (set $mod ,sway-mod)
            (set $term ,default-terminal)
            (set $backup-term ,backup-terminal)
            (set $menu ,default-application-launcher)
            (set $lock ,lock-cmd)

            ,@(if (get-value 'default-pass-prompt-fn config)
                  `((set $pass ,(get-value-eval 'default-pass-prompt-fn config))
                    (bindsym --to-code $mod+Shift+p exec $pass))
                  '())

            (floating_modifier $mod normal)

            (bindsym --to-code $mod+Shift+r reload)
            ,@(if (get-value 'dbus config)
                  `((,#~"\n\n# Update dbus environment variables:")
                    (exec ,(file-append
                            (get-value 'dbus config)
                            "/bin/dbus-update-activation-environment")
                          WAYLAND_DISPLAY XDG_CURRENT_DESKTOP))
                  '())
            (bindsym --to-code $mod+Shift+q
                     exec ,#~(string-append
                              #$rofi "/bin/rofi -show p -modi p:"
                              #$rofi-power-menu "/rofi-power-menu"))

            (,#~"\n\n# Launching external applications:")
            (bindsym $mod+Control+Shift+Return exec $backup-term)
            (bindsym $mod+Return exec $term)

            (bindsym --to-code $mod+Space exec $menu)
            ;;(bindsym --to-code $mod+Shift+l exec $lock)

            (,#~"\n\n# Manipulating windows:")
            (bindsym --to-code $mod+q kill)
            (bindsym --to-code $mod+f fullscreen)

            (bindsym $mod+Shift+space floating toggle)
            (bindsym $mod+Ctrl+space focus mode_toggle)
            ;; (bindsym $mod+grave floating toggle)
            ;; (bindsym $mod+Shift+grave focus mode_toggle)

            (set $left h)
            (set $right l)
            (set $up k)
            (set $down j)

            (bindsym $mod+$up focus prev)
            (bindsym $mod+$down focus next)
            ;; ($mod+Tab layout toggle split tabbed)
            ;; ($mod+Shift+Tab split toggle)

            (bindsym $mod+Shift+$up move left)
            (bindsym $mod+Shift+$down move right)
            (bindsym $mod+o move up)
            (bindsym $mod+Shift+o move down)

            (,#~"\n\n# Moving around workspaces:")
            (bindsym $mod+tab workspace back_and_forth)
            (bindsym --to-code $mod+ampersand workspace 1)
            (bindsym --to-code $mod+eacute workspace 2)
            (bindsym --to-code $mod+quotedbl workspace 3)
            (bindsym --to-code $mod+apostrophe workspace 4)
            (bindsym --to-code $mod+parenleft workspace 5)
            (bindsym --to-code $mod+minus workspace 6)
            (bindsym --to-code $mod+egrave workspace 7)
            (bindsym --to-code $mod+underscore workspace 8)
            (bindsym --to-code $mod+ccedilla workspace 9)
            (bindsym --to-code $mod+agrave workspace 10)
            (bindsym --to-code $mod+Shift+ampersand move container to workspace 1)
            (bindsym --to-code $mod+Shift+eacute move container to workspace 2)
            (bindsym --to-code $mod+Shift+quotedbl move container to workspace 3)
            (bindsym --to-code $mod+Shift+apostrophe move container to workspace 4)
            (bindsym --to-code $mod+Shift+parenleft move container to workspace 5)
            (bindsym --to-code $mod+Shift+minus move container to workspace 6)
            (bindsym --to-code $mod+Shift+egrave move container to workspace 7)
            (bindsym --to-code $mod+Shift+underscore move container to workspace 8)
            (bindsym --to-code $mod+Shift+ccedilla move container to workspace 9)
            (bindsym --to-code $mod+Shift+agrave move container to workspace 10)

            (,#~"\n\n# Scratchpad settings:")
            ;;(bindsym --to-code $mod+Shift+minus move scratchpad)
            ;;(bindsym --to-code $mod+minus scratchpad show)

	    (,#~"")
            (default_border pixel)
            (default_floating_border pixel)
            (gaps inner ,(get-value 'emacs-margin config 8))

            ))))

       (when (get-value 'swayidle-cmd config)
         (simple-service
	  'sway-enable-swayidle
	  home-sway-service-type
          `((,#~"")
	    (exec ,(get-value 'swayidle-cmd config)))))

       (when (get-value 'swayidle config)
         (let* ((swaymsg (file-append sway "/bin/swaymsg"))
                (swaymsg-cmd (lambda (cmd)
                               #~(format #f "'~a \"~a\"'" #$swaymsg #$cmd)))
                (idle-timeout (+ 30 (get-value 'lock-timeout config 120))))
           (simple-service
            'sway-add-dpms-to-swayidle
            home-swayidle-service-type
            `((timeout ,idle-timeout ,(swaymsg-cmd "output * dpms off")
                       resume                ,(swaymsg-cmd "output * dpms on"))))))

       (when (get-value 'swayr config)
         (let* ((swayr-cmd (get-value 'swayr-cmd config)))
           (simple-service
            'swayr-configuration
            home-sway-service-type
            `((,#~"") ;; enable swayrd
              (exec ,(get-value 'swayrd-cmd config))
              (,#~"")
              (bindsym --to-code $mod+Shift+$left
                       ,(swayr-cmd "next-window all-workspaces"))
              (bindsym --to-code $mod+Shift+$right
                       ,(swayr-cmd "prev-window all-workspaces"))
              (bindsym --to-code $mod+Escape
                       ,(swayr-cmd "switch-window"))
              (bindsym --to-code $mod+Delete
                       ,(swayr-cmd "quit-window"))
              (bindsym --to-code $mod+Tab
                       ,(swayr-cmd "switch-to-urgent-or-lru-window"))
              (bindsym --to-code $mod+Shift+Space
                       ,(swayr-cmd "switch-workspace-or-window"))
              (bindsym --to-code $mod+c
                       ,(swayr-cmd "execute-swaymsg-command"))
              (bindsym --to-code $mod+Shift+c
                       ,(swayr-cmd "execute-swayr-command"))))))

       (simple-service
	'sway-configuration
	home-sway-service-type
        `(,@extra-config
	  (,#~"")))

       (simple-service
        'sway-reload-config-on-change
        home-run-on-change-service-type
        `(("files/.config/sway/config"
           ,#~(system* #$(file-append sway "/bin/swaymsg") "reload"))))

       (simple-service
        'packages-for-sway
	home-profile-service-type
        (append
         (if (and (get-value 'default-terminal config)
                  (get-value 'backup-terminal config))
             '() (list foot))
         (if (get-value 'default-application-launcher config) '() (list bemenu))
	 (list qtwayland-5 swayhide
               xdg-desktop-portal xdg-desktop-portal-wlr)))
       (simple-service 'set-wayland-specific-env-vars
		       home-environment-variables-service-type
		       ;; export NO_AT_BRIDGE=1
		       '(("XDG_CURRENT_DESKTOP" . "sway")
                         ("XDG_SESSION_TYPE" . "wayland")
                         ;; FIXME: Should be in feature-pipewire
                         ("RTC_USE_PIPEWIRE" . "true")
                         ("SDL_VIDEODRIVER" . "wayland")
                         ("MOZ_ENABLE_WAYLAND" . "1")
                         ("CLUTTER_BACKEND" . "wayland")
                         ("ELM_ENGINE" . "wayland_egl")
                         ("ECORE_EVAS_ENGINE" . "wayland-egl")
                         ("QT_QPA_PLATFORM" . "wayland-egl")
			 ("_JAVA_AWT_WM_NONREPARENTING" . "1"))))))

  (feature
   (name 'sway)
   (values `((sway . ,sway)
             (wl-clipboard . ,wl-clipboard)
	     (wayland . #t)
             (xwayland? . ,xwayland?)))
   (home-services-getter sway-home-services)))
