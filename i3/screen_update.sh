THISHOST=$(hostname)
intern=eDP1
extern=HDMI-1



if [ "$THISHOST" == "quimera" ]; then
    xrandr --output VGA1 --left-of HDMI1

elif xrandr | grep "$extern disconnected"; then
    xrandr --output "$extern" --off --output "$intern" --auto

else
    xrandr --output "$extern" --output "HDMI1" --mode 1920x1080 --left-of "eDP1"
    xrandr --output "$extern" --output "HDMI-1" --mode 1920x1080 --left-of "eDP-1"
fi
