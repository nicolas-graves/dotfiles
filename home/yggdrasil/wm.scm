(define-module (home yggdrasil wm)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home-services wm)
  #:use-module (home services i3blocks)
  #:use-module (home services mako)
  #:use-module (home services swappy)
  #:use-module (gnu packages gnupg)
  #:use-module (kreved packages wm))

(define-public services
  (list
   (service
    home-sway-service-type
    (home-sway-configuration
     (config
      `((set $mod Mod4)
        (set $left h)
        (set $right l)
        (set $up k)
        (set $down j)

        (set $term alacritty)
        (set $menu bemenu-run
             --prompt "'run:'"
             --ignorecase)

        (bindsym
         --to-code
         (($mod+Return exec $term)
          ($mod+space exec $menu)
          ($mod+q kill)
          ($mod+0 reload)
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
          ($mod+g exec makoctl dismiss --all)
          ($mod+m exec makoctl set-mode dnd)
          ($mod+Shift+m exec makoctl set-mode default)
          (XF86MonBrightnessUp exec light -A 10)
          (XF86MonBrightnessDown exec light -U 10)
          ($mod+ampersand workspace 1)
          ($mod+eacute workspace 2)
          ($mod+quotedbl workspace 3)
          ($mod+apostrophe workspace 4)
          ($mod+parenleft workspace 5)
          ($mod+minus workspace 6)
          ($mod+egrave workspace 7)
          ($mod+underscore workspace 8)
          ($mod+ccedilla workspace 9)
          ($mod+agrave workspace 10)
          ($mod+Shift+ampersand move container to workspace 1)
          ($mod+Shift+eacute move container to workspace 2)
          ($mod+Shift+quotedbl move container to workspace 3)
          ($mod+Shift+apostrophe move container to workspace 4)
          ($mod+Shift+parenleft move container to workspace 5)
          ($mod+Shift+minus move container to workspace 6)
          ($mod+Shift+egrave move container to workspace 7)
          ($mod+Shift+underscore move container to workspace 8)
          ($mod+Shift+ccedilla move container to workspace 9)
          ($mod+Shift+agrave move container to workspace 10)))

        (bindsym
         --locked
         ((XF86MonBrightnessUp exec light -A 10)
          (XF86MonBrightnessDown exec light -U 10)))

        (exec swayidle -w
              before-sleep "'swaylock -f'"
              timeout 1800 "'swaylock -f'"
              timeout 2400 "'swaymsg \"output * dpms off\"'"
              resume "'swaymsg \"output * dpms on\"'")
        (exec wlsunset -l 50.6 -L 36.6 -T 6500 -t 3000)
        (exec mako)

        (xwayland disable)
        (workspace_auto_back_and_forth yes)
        (focus_follows_mouse no)
        (smart_borders on)
        (title_align center)


        (output * bg /home/graves/.dotfiles/home/yggdrasil/files/fond_pre.jpg fill)
        (output eDP-1 scale 1)

        (input *
               ((xkb_layout fr)
                (repeat_delay 300)
                (repeat_rate 50)))

        (assign "[app_id=\"nyxt\"]" 2)
        (assign "[app_id=\"chromium-browser\"]" 2)
        (assign "[app_id=\"emacs\"]" 3)

        (for_window
         "[app_id=\"^.*\"]"
         inhibit_idle fullscreen)
        (for_window
         "[title=\"^(?:Open|Save) (?:File|Folder|As).*\"]"
         floating enable, resize set width 70 ppt height 70 ppt)

        ;;(font "Iosevka, Light 13")
        (client.focused "#EEEEEE" "#005577" "#770000" "#770000" "#770000")
        (client.unfocused "#BBBBBB" "#222222" "#444444")
        (default_border pixel 4)
        (default_floating_border none)
        (gaps inner 8)
        (seat * xcursor_theme Adwaita 24)

        (bar
         ((status_command i3blocks)
          (position top)
          (separator_symbol "|")
          (font "Iosevka 12.5")
          (pango_markup enabled)
          (colors
           ((statusline "#FFFFFF")
            (background "#000000")
            (focused_workspace "#81A1C1" "#81A1C1" "#f0f0f0")
            (inactive_workspace "#595959" "#595959" "#ffffff")))))))))

   (service
    home-i3blocks-service-type
    (home-i3blocks-configuration
     (config
      `((battery1
         ((command . ,(local-file "scripts/battery" #:recursive? #t))
          (BAT_NUM . 1)
          (interval . 10)))
        (battery0
         ((command . ,(local-file "scripts/battery" #:recursive? #t))
          (BAT_NUM . 0)
          (interval . 10)))
        (date
         ((command . "date '+%a, %d %b'")
          (interval . 1)))
        (time
         ((command . "date +%H:%M")
          (interval . 1)))))))

   (service
    home-mako-service-type
    (home-mako-configuration
     (package mako-next)
     (config
      `((sort . -time)
        (actions . 0)
        (icons . 0)
        (font . "Iosevka Light 14")
        (text-color . "#000000")
        (background-color . "#FFFFFF")
        (border-color . "#721045")
        (layer . overlay)
        (border-size . 2)
        (padding . 10)
        (width . 400)
        (group-by . app-name)
        (ignore-timeout . 1)
        (default-timeout . 3500)
        ((mode dnd)
         .
         ((invisible . 1)))))))

   (service
    home-swappy-service-type
    (home-swappy-configuration
     (config
      `((Default
          ((show_panel . true)
           (save_dir . $HOME/img)
           (save_filename_format . scrot-%Y%m%d-%H%M%S.png)))))))))
