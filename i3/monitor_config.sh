#!/bin/bash

THISHOST=$(hostname)

#1st Escale notebook

if [ "$THISHOST" == "scadulfax" ] ; then

    xrandr --auto
    xrandr --output HDMI-1-1 --off

    CONNECTED=$(xrandr | grep " connected " | wc -l)
    echo $CONNECTED
    if [ "$CONNECTED" -ge "2" ]; then
        xrandr --output DVI-I-2-1 --above eDP-1-1
        xrandr --output DVI-I-2-1 --auto

        xrandr --output eDP-1-1 --auto

        xrandr --output HDMI-1-2 --right-of DVI-I-2-1
        xrandr --output HDMI-1-2 --auto
    else
        xrandr --output HDMI-1-1 --off
        xrandr --output eDP-1-1 --auto
    fi
fi
