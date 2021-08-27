(define-module (home yggdrasil wm)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services wm))

(define ws-bindings
  (map (lambda (ws)
         `(,(string->symbol (format #f "$mod+~d" ws))
           workspace number ,ws))
       (iota 9 1)))

(define ws-move-bindings
  (map (lambda (ws)
         `(,(string->symbol (format #f "$mod+Shift+~d" ws))
           move container to workspace number ,ws))
       (iota 9 1)))

(define-public services
  (list
   (service
    home-sway-service-type
    (home-sway-configuration
     (config
      `((set $mod Mod4)
        (set $left b)
        (set $right f)
        (set $up p)
        (set $down n)

        (set $term alacritty)
        (set $menu bemenu-run
             --prompt "'run:'"
             --ignorecase
             --fn "'Iosevka Light 14'"
             --nb "'#FFFFFF'" --nf "'#000000'"
             --tb "'#FFFFFF'" --tf "'#000000'"
             --fb "'#FFFFFF'" --ff "'#000000'"
             --hb "'#F0F0F0'" --hf "'#721045'")

        (bindsym
         --to-code
         (($mod+Return exec $term)
          ($mod+space exec $menu)
          ($mod+c kill)
          ($mod+q reload)
          ($mod+Shift+q exec swaymsg exit)
          ($mod+$up focus prev)
          ($mod+$down focus next)
          ($mod+Shift+$left move left)
          ($mod+Shift+$right move right)
          ($mod+Shift+$up move up)
          ($mod+Shift+$down move down)
          ($mod+f fullscreen)
          ($mod+Tab layout toggle split tabbed)
          ($mod+Shift+Tab split toggle)
          ($mod+grave floating toggle)
          ($mod+Shift+grave focus mode_toggle)
          ($mod+Shift+s exec "grim -g \"$(slurp)\" - | swappy -f -")
          (Print exec "grim - | wl-copy -t image/png")
          ;; (XF86MonBrightnessUp exec light -A 10)
          ;; (XF86MonBrightnessDown exec light -U 10)
          ,@ws-bindings
          ,@ws-move-bindings))

        (exec swayidle -w
              before-sleep "swaylock -f"
              timeout 1800 "swaylock -f"
              timeout 2400 "swaymsg \"output * dpms off\""
              resume "swaymsg \"output * dpms on\"")
        (exec mako)

        (xwayland disable)
        (workspace_auto_back_and_forth yes)
        (focus_follows_mouse no)
        (smart_borders on)
        (title_align center)

        (output * bg ,(local-file "files/wp.jpg") fill)
        (output eDP-1 scale 2)

        (input type:keyboard
               ((xkb_layout us,ru)
                (xkb_options grp:toggle,ctrl:swapcaps)))
        (input type:touchpad events disabled)
        (input "2:10:TPPS/2_IBM_TrackPoint"
               ((accel_profile flat)))

        (assign "[app_id=\"Chromium-browser\"]" 2)
        (assign "[app_id=\"emacs\"]" 3)
        (assign "[app_id=\"telegramdesktop\"]" 4)

        (for_window
         "[app_id=\"telegramdesktop\" title=\"Media viewer\"]"
         focus)
        (for_window
         "[class=\"^.*\"]"
         inhibit_idle fullscreen)
        (for_window
         "[app_id=\"^.*\"]"
         inhibit_idle fullscreen)

        (font "Iosevka, Light 14")
        (client.focused "#f0f0f0" "#f0f0f0" "#721045" "#721045" "#721045")
        (client.unfocused "#ffffff" "#ffffff" "#595959")
        (default_border pixel 4)
        (default_floating_border none)

        (bar
         (;(status_command i3status)
          (position top)
          (separator_symbol "|")
          (font "Iosevka, Light 14")
          (pango_markup enabled)
          (colors
           ((statusline "#000000")
            (background "#FFFFFF")
            (focused_workspace "#f0f0f0" "#f0f0f0" "#721045")
            (inactive_workspace "#ffffff" "#ffffff" "#595959")))))))))))
