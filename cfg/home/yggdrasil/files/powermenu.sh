#!/bin/sh

case $1 in
    logout)
        swaymsg exit
        ;;
    reboot)
        loginctl reboot
        ;;
    poweroff)
        loginctl poweroff
        ;;
esac
