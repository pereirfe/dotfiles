#!/bin/bash

THISHOST=$(hostname)

#1st Escale notebook
if [ "$THISHOST" == "nunavut" ]; then
    CONNECTED=$(xrandr | grep " connected " | wc -l)

    if [ "$CONNECTED" -eq "3" ]; then
        xrandr --output eDP-1 --off
        xrandr --output DP-1 --auto
        xrandr --output HDMI-1 --auto
        xrandr --output HDMI-1 --right-of DP-1

    else
        xrandr --output HDMI-1 --off
        xrandr --output DP-1 --off
        xrandr --output eDP-1 --auto
    fi
fi
