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

(define-module (home features wm)
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
  #:use-module (home services wm)
  #:use-module (home packages swayr)

  #:use-module (guix gexp)
  #:use-module (guix packages)

  #:use-module (srfi srfi-1)

  #:export (feature-sway
	    feature-sway-run-on-tty
            feature-sway-screenshot

            feature-sway-statusbar
            feature-waybar
            waybar-sway-language
            waybar-sway-window
            waybar-sway-workspaces
            waybar-tray
            waybar-temperature
            waybar-idle-inhibitor
            waybar-clock
            waybar-battery

            feature-swayidle
            feature-swaylock
            feature-swayr))


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

(define* (feature-sway
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

            (floating_modifier $mod normal)

            (bindsym --to-code $mod+Shift+r reload)
            (bindsym --to-code $mod+Shift+q exec swaymsg exit)

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
         (simple-service
          'sway-enable-swayrd
          home-sway-service-type
          `((,#~"")
            (exec ,(get-value 'swayrd-cmd config))))

         (let* ((swayr-cmd (get-value 'swayr-cmd config)))
           (simple-service
            'swayr-configuration
            home-sway-service-type
            `((,#~"")
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
	 (list qtwayland swayhide
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


;;;
;;; sway-run-on-tty.
;;;

(define* (feature-sway-run-on-tty
          #:key
          (sway-tty-number 2)
          (launch-arguments ""))
  "Launch Sway on specified tty upon user login.  Also,
automatically switch to SWAY-TTY-NUMBER on boot."
  (ensure-pred tty-number? sway-tty-number)
  (ensure-pred string? launch-arguments)

  (define (sway-run-on-tty-home-services config)
    (require-value 'sway config)
    (list
     (simple-service
      'run-sway-on-login-to-sway-tty
      home-shell-profile-service-type
      (list
       #~(format #f "[ $(tty) = /dev/tty~a ] && exec ~a~a~a"
                 #$sway-tty-number
                 #$(file-append (get-value 'sway config) "/bin/sway")
                 #$(if (positive? (string-length launch-arguments)) " " "")
                 #$launch-arguments)))))

  (define (sway-run-on-tty-system-services _)
    (list
     (simple-service
      'switch-to-sway-tty-after-boot shepherd-root-service-type
      (list (shepherd-service
             (provision '(switch-to-sway-tty))
             (requirement '(virtual-terminal))
             (start #~(lambda ()
			(invoke #$(file-append kbd "/bin/chvt")
				#$(format #f "~a" sway-tty-number))))
             (one-shot? #t))))))

  (feature
   (name 'sway-run-on-tty)
   (values (make-feature-values sway-tty-number))
   (home-services-getter sway-run-on-tty-home-services)
   (system-services-getter sway-run-on-tty-system-services)))


;;;
;;; sway-screenshot.
;;;

(define* (feature-sway-screenshot)
  "Configure slurp, grim and other tools for screenshot capabilities.  Feature
is sway dependent, because it relies on swaymsg."

  (define sway-f-name 'screenshot)
  (define f-name (symbol-append 'sway- sway-f-name))

  (define (get-home-services config)
    (require-value 'sway config)
    (define subject-output
      #~(format #f "~a -t get_outputs | ~a -r '.[] | select(.focused) | .name'"
                #$(file-append (get-value 'sway config) "/bin/swaymsg")
                #$(file-append jq "/bin/jq")))
    (define subject-window-or-selection
      #~(format #f "~a -t get_tree | ~a -r '.. | select(.pid? and .visible?) \
| .rect | \"\\(.x),\\(.y) \\(.width)x\\(.height)\"' | ~a -b ~a -B ~a"
                #$(file-append (get-value 'sway config) "/bin/swaymsg")
                #$(file-append jq "/bin/jq")
                ;; TODO: Move to slurp-cmd
                #$(file-append slurp "/bin/slurp")
                "303030AA"
                "303030AA"))

    (define* (shot-script subject #:key output geom (file "-"))
      (program-file
       (string-append "sway-shot-" subject)
       #~(system
          (format #f "~a ~a~a~a | ~a"
                  #$(file-append grim "/bin/grim")
                  #$(if output #~(string-append "-o \"$(" #$output ")\" ") "")
                  #$(if geom #~(string-append "-g \"$(" #$geom ")\" ") "")
                  #$file
                  #$(file-append (get-value 'wl-clipboard config)
                                 "/bin/wl-copy")))))

    (define shot-output
      (shot-script "output" #:output subject-output))
    (define shot-window-or-selection
      (shot-script "window-or-selection" #:geom subject-window-or-selection))
    (define swappy-clipboard
      (program-file
       "sway-swappy-clipboard"
       #~(system
          (format #f "~a | ~a -f -"
                  #$(file-append (get-value 'wl-clipboard config wl-clipboard)
                                 "/bin/wl-paste")
                  #$(file-append (get-value 'swappy config swappy)
                                 "/bin/swappy")))))
    (list
     (simple-service
      'sway-screenshot
      home-sway-service-type
      `((bindsym $mod+Print exec ,shot-output)
        (bindsym $mod+Alt+Print exec ,swappy-clipboard)
        (bindsym $mod+Shift+Print exec ,shot-window-or-selection)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


;;;
;;; sway-statusbar.
;;;

;; <https://www.reddit.com/r/unixporn/comments/a2c9kl/sway_in_the_wild/>
(define* (feature-sway-statusbar
          #:key
          (battery "BAT0")
          (use-global-fonts? #f))
  "Configure statusbar."

  (define sway-f-name 'statusbar)
  (define f-name (symbol-append 'sway- sway-f-name))

  (define (get-home-services config)
    (require-value 'sway config)
    (when use-global-fonts?
      (require-value 'font-monospace config))
    (define font-mono (get-value 'font-monospace config))
    (define (get-status-command)
      (format #f
              "while echo $(cat /sys/class/power_supply/~a/capacity)% \
$(date +'%Y-%m-%d %l:%M:%S %p'); do sleep 5; done" battery))
    (list
     (simple-service
      'sway-statusbar
      home-sway-service-type
      `((bar ((position top)
              ,@(if use-global-fonts?
                    `((font ,(font-name font-mono)))
                    '())
              (colors ((statusline "#ffffff")
                       (background "#323232")))
              (status_command ,(get-status-command))))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


;;;
;;; waybar.
;;;

;; TODO: Move to home services?
(define* (waybar-module
          name
          #:optional
          (config '())
          (style '())
          #:key
          (placement 'modules-right)
          (bar-id 'main))
  "Returns a service, which extends home-waybar-service-type in the way the
module will be added to the BAR-ID."
  (simple-service
   (symbol-append 'waybar-module- name)
   home-waybar-service-type
   (home-waybar-extension
    (config `#(((name . ,bar-id)
                (,placement . #(,name))
                (,name . ,config))))
    (style-css style))))

(define* (waybar-sway-language)
  (waybar-module 'sway/language))

(define* (waybar-sway-window)
  (waybar-module
   'sway/window
   `()
   `((#{#window}#
      ((margin-left . 1em)
       (margin-right . 1em))))
   #:placement 'modules-center))

(define* (waybar-sway-workspaces
          #:key
          (persistent-workspaces '())
          (format-icons '(("1" . )
                          ("2" . )
                          ("3" . )
                          ("4" . )
                          ("6" . )  ; 
                          ("7" . )  ; 
                          ("8" . )
                          ("9" . )
                          ("10" . )

                          ("urgent" . )
                          ("focused" . )
                          ("default" . ))))
  "PERSISTENT-WORKSPACES is a list of pairs workspace and vector of outputs."
  (waybar-module
   'sway/workspaces
   `((disable-scroll . #t)
     (format . {icon})
     ;; FIXME: Height becomes higher when icons are not used.
     (format-icons . ,format-icons)
     (persistent_workspaces . ,persistent-workspaces))
   `(((#{#workspaces}# button)
      ((background . none)
       (border-radius . 0.2em)
       (margin . (0.4em 0.2em))
       (padding . (0.1em 0.2em))
       (color . @base05)))

     ((#{#workspaces}# button:hover)
      ((background . none)
       (border-color . @base07)))

     ((#{#workspaces}# button.focused)
      ((background . @base02)
       (color . @base07)))

     ((#{#workspaces}# button.urgent)
      ((color . @base08))))
   #:placement 'modules-left))

(define (waybar-tray)
  (waybar-module
   'tray
   `()
   `(((#{#tray}# menu)
      ((color . @base05)
       (background . @base01)
       (border . (solid 1px))
       (border-color . @base02)))

     ((#{#tray}# menu menuitem)
      ((padding-top . 0px)
       (padding-bottom . 0px)
       (margin-top . 0.1em)
       (margin-bottom . 0em)))

     ((#{#tray}# menu menuitem:hover)
      ((background . none)))

     ((#{#tray}# menu separator)
      ((background . @base03)
       (padding-top . 1px)
       (margin-top . 0.2em)
       (margin-bottom . 0.2em))))))

(define* (waybar-temperature) (waybar-module 'temperature))

(define* (waybar-idle-inhibitor)
  (waybar-module
   'idle_inhibitor
   '((format . {icon})
     (format-icons . ((activated . )
                      (deactivated . ))))))

(define* (waybar-clock
          #:key
          (format "{:%Y-%m-%d %H:%M}")
          (interval 60)
          (timezone #f))
  "Returns a function, which accepts rde config and returns waybar clock module.
Left click on the module open world-clock in emacs client, right click opens
calendar."
  (lambda (config)
    (define ec (get-value 'emacs-client config))
    (define (ec-command command)
      #~(format #f "\"~a --eval \\\"(~a)\\\"\"" #$ec #$command))
    (waybar-module
     'clock
     `((tooltip-format
        . "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>")

       ,@(if timezone `((timezone . ,timezone)) '())
       (format . ,format)
       ,@(if ec
             `((on-click . ,(ec-command "world-clock"))
               (on-click-right . ,(ec-command "calendar")))
             '())

       (interval . ,interval)))))

(define* (waybar-battery
          #:key
          (intense? #f)
          (charging-icon "⚡"))
  "When INTENSE? is #t changes background color instead of text color when the
battery is low or nearly empty."
  (waybar-module
   'battery
   `((format . "{capacity}% {icon}")
     (format-charging . ,(format #f "{capacity}~a {icon}" charging-icon))
     ;; | icon |  range |
     ;; |------+--------|
     ;; |    0 | 0-10   |
     ;; |   25 | 10-40  |
     ;; |   50 | 40-60  |
     ;; |   75 | 60-90  |
     ;; |  100 | 90-100 |
     (states . ((empty . 10)
                (low . 20)
                (half-low . 40)
                (half . 60)
                (high . 90)
                (full . 100)))
     (format-icons . ((empty . )
                      (low . )
                      (half-low . )
                      (half . )
                      (high . )
                      (full . ))))
   `((#{#battery.discharging.empty}#
      ,(if intense?
           `((color . @base02)
             (background . @base08))
           `((color . @base08))))
     (#{#battery.discharging.low}#
      ,(if intense?
           `((color . @base02)
             (background . @base09))
           `((color . @base09)))))))

(define* (feature-waybar
          #:key
          (waybar waybar)
          (waybar-modules
           (list
            (waybar-sway-workspaces)
            (waybar-tray)
            (waybar-idle-inhibitor)
            (waybar-sway-language)
            (waybar-battery #:intense? #f)
            (waybar-clock)))
          (base16-css (local-file "../../../.local/src/rde/rde/features/wm/waybar/base16-default-dark.css"))
          (transitions? #f))
  "Configure waybar.  Each element of WAYBAR-MODULES is a home service or a
function accepting an rde config and returning a home-service, which extends
home-waybar-service-type.  Set TRANSITIONS? to #t if you prefer a smooth
animation."

  (define f-name 'waybar)

  (define (get-home-services config)
    ;; (when use-global-fonts?
    ;;   (require-value 'font-monospace config))
    (define font-mono
      (and=> (get-value 'font-monospace config)
             (compose string->symbol font-name)))
    (append
     (list
      (service
        home-waybar-service-type
        (home-waybar-configuration
         (waybar waybar)
         (config #(((position . top)
                    (name . main))))
         ;; TODO: fix tray menu styles.
         (style-css
          `(,#~(format #f "@import \"~a\";\n" #$base16-css)
            (*
             ((font-family . #(,@(if font-mono (list font-mono) '())
                               ;; TODO: Add icon-font argument
                               FontAwesome))
              ,@(if transitions? '() '((transition . none)))
              (box-shadow . none)
              (text-shadow . none)
              (min-height . 0)))

            (tooltip
             ((border . (solid @base02))
              (background . @base01)
              (opacity . 0.9)))

            ((tooltip label)
             ((color . @base05)
              (padding . 0)))

            (#{#waybar}#
             ((color . @base04)
              (background . @base01)))

            (#((.modules-right label)
               (.modules-right image))
             ((margin . (0.4em 0.2em))
              (padding . (0 0.4em))
              (background . @base02)
              (border-radius . 0.2em)))

            (.modules-left
             ((margin-left . 0.2em)))

            (.modules-right
             ((margin-right . 0.2em))))))))

      (map (lambda (x) (if (procedure? x) (x config) x)) waybar-modules)

      (list
       (simple-service
        'waybar-add-font-package
        home-profile-service-type
        (list font-awesome))

       (simple-service
        'waybar-reload-config-on-change
        home-run-on-change-service-type
        `(("files/.config/waybar/style.css"
           ,#~(system* #$(file-append psmisc "/bin/killall") "-SIGUSR2" "waybar"))
          ("files/.config/waybar/config"
           ,#~(system* #$(file-append psmisc "/bin/killall") "-SIGUSR2" "waybar"))))

       (when (get-value 'sway config)
         (simple-service
          'sway-waybar
          home-sway-service-type
          `((bar swaybar_command ,(file-append waybar "/bin/waybar"))))))))

  (feature
   (name 'waybar)
   (values `((waybar . ,waybar)
             (sway-statusbar . #t)))
   (home-services-getter get-home-services)))


;;;
;;; swayidle.
;;;

(define* (feature-swayidle
          #:key
          (swayidle swayidle)
          (lock-timeout 240)
          (extra-config '()))
  "Configure swayidle."
  (ensure-pred any-package? swayidle)

  (define swayidle-cmd (file-append swayidle "/bin/swayidle -w"))

  (define (get-home-services config)
    (define lock-cmd (get-value 'default-screen-locker config))

    (list
     (service
      home-swayidle-service-type
      (home-swayidle-configuration
       (swayidle swayidle)
       (config
        `(,@(if lock-cmd
                (let ((lock-cmd-quoted (format #f "'~a'" lock-cmd)))
                  `((lock ,lock-cmd-quoted)
                    (before-sleep ,lock-cmd-quoted)
                    (timeout ,lock-timeout ,lock-cmd-quoted)))
                '())
          ,@extra-config))))))

  (feature
   (name 'swayidle)
   (values `((swayidle . ,swayidle)
             (swayidle-cmd . ,swayidle-cmd)
             (lock-timeout . ,lock-timeout)))
   (home-services-getter get-home-services)))


;;;
;;; swaylock.
;;;

(define* (feature-swaylock
          #:key
          (swaylock swaylock)
          (show-failed-attempts? #t)
          (show-keyboard-layout? #f)
          (daemonize? #t)
          (extra-config '())
          (default-screen-locker? #t))
  "Configure swaylock."
  (ensure-pred any-package? swaylock)

  (define (get-home-services config)
    (list
     (service
      home-swaylock-service-type
      (home-swaylock-configuration
       (swaylock swaylock)
       (config
        `((show-failed-attempts . ,show-failed-attempts?)
          (daemonize . ,daemonize?)
          (show-keyboard-layout . ,show-keyboard-layout?)
          ;; TODO: Source color from colorscheme
          (color . 3e3e3e)
          (indicator-caps-lock)
          ,@extra-config))))))

  (define (get-system-services _)
    (list
     (screen-locker-service swaylock "swaylock")
     ;; (simple-service
     ;;  'setuid-chkpwd
     ;;  setuid-program-service-type
     ;;  (list (file-like->setuid-program
     ;;         (file-append linux-pam "/sbin/unix_chkpwd"))))

     ;; (simple-service
     ;;  'sway-add-swaylock-pam
     ;;  pam-root-service-type
     ;;  (list
     ;;   (unix-pam-service "swaylock")))
     ))

  (feature
   (name 'swaylock)
   (values `((swaylock . ,swaylock)
             ,@(if default-screen-locker?
                   ;; TODO: Change it to path in the store, once
                   ;; https://issues.guix.gnu.org/53468 is resolved
                   `((default-screen-locker . "/run/setuid-programs/swaylock"))
                   '())))
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))


;;;
;;; swayr.
;;;

(define* (feature-swayr
          #:key
          (swayr swayr))
  "Configure swayr."
  (ensure-pred any-package? swayr)

  (define swayrd-cmd
    (let* ((log-file
            (list
             (string-append (or (getenv "XDG_LOG_HOME")
                                (format #f "~a/.local/var/log"
                                        (getenv "HOME")))
                            "/swayrd.log")))
           (cmd '("swayrd"))
           (environment-variables
            '("RUST_BACKTRACE=1" "RUST_LOG=swayr=debug")))
      (string-join
       (append '("env") environment-variables
               cmd '(">>") log-file '("2>&1")) " ")))

  (define (swayr-cmd cmd)
    (let* ((log-file
            (list
             (string-append (or (getenv "XDG_LOG_HOME")
                                (format #f "~a/.local/var/log"
                                        (getenv "HOME")))
                            "/swayr.log")))
           (environment-variables '("RUST_BACKTRACE=1")))
      (string-join
       (append '("exec" "env") environment-variables '("swayr")
               (list cmd) '(">>") log-file '("2>&1")) " ")))

  (define (get-home-services config)
    (list
     (service
      home-swayr-service-type
      (home-swayr-configuration
       (package swayr)
       (config
        '((menu
           ((executable . "rofi")
            (args . ("-dmenu" "-i" "-p" "window"))))
          (format
           ((output-format . "Output {name}")
            (workspace-format
             . "Workspace {name} [{layout}] on output {output_name}")
            (container-format
             . "Container [{layout}] {marks} on workspace {workspace_name}")
            (window-format
             . "{app_name} — “{title}” {marks} on workspace {workspace_name}"))
           )))))))

  (feature
   (name 'swayr)
   (values `((swayr . ,swayr)
             (swayrd-cmd . ,swayrd-cmd)
             (swayr-cmd . ,swayr-cmd)))
   (home-services-getter get-home-services)))

;; [X] feature-sway-run-on-tty
;; [X] feature-sway-screenshot
;; [X] feature-sway-lock-idle-sleep
;; [ ] feature-sway-input
;; [ ] feature-sway-keybindings
;; [ ] feature-sway-media-keys
;; [ ] feature-sway-outputs (kanshi, workspaces, displays)

;; [ ] feature-wayland-appearance (sway, gtk, qt themes)
;; [ ] feature-wayland-statusbar
;; [ ] feature-wayland-notifications
;; [ ] feature-wayland-clipboard


;; window rules will configured on app's feature basis
