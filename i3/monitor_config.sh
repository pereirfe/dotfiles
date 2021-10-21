#!/bin/bash

THISHOST=$(hostname)

#1st Escale notebook

if [ "$THISHOST" == "scadulfax" ] ; then
    xmodmap ~/.Xmodmap
    xrandr --auto
    xrandr --output HDMI-1-1 --off
    xrandr --output eDP-1-1 --off
    xrandr --output DVI-I-2-1 --left-of HDMI-1-2 --mode 1920x1080

    # xrandr --auto
    # xrandr --output HDMI-1-1 --off
    # xrandr --output DVI-I-2-1 --left-of eDP-1-1 --mode 1920x1080
    # xrandr --output eDP-1-1 --pos 1920x0
    # xrandr --output HDMI-1-2 --right-of eDP-1-1



    # CONNECTED=$(xrandr | grep " connected " | wc -l)
    # echo $CONNECTED
    # if [ "$CONNECTED" -ge "2" ]; then
    #     xrandr --output DVI-I-2-1 --above eDP-1-1
    #     xrandr --output DVI-I-2-1 --auto

    #     xrandr --output eDP-1-1 --auto

    #     xrandr --output HDMI-1-2 --right-of DVI-I-2-1
    #     xrandr --output HDMI-1-2 --auto
    # else
    #     xrandr --output HDMI-1-1 --off
    #     xrandr --output eDP-1-1 --auto
    # fi
fi
