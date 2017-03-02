THISHOST=$(hostname)

if [ "$THISHOST" == "quimera" ]; then
    xrandr --output VGA1 --left-of HDMI1
fi
