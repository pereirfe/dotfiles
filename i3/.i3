# This file has been auto-generated by i3-config-wizard(1).
# It will not be overwritten, so edit it as you like.
#
# Should you change your keyboard layout some time, delete
# this file and re-run i3-config-wizard(1).
#
# i3 config file (v4)
#
# Please see http://i3wm.org/docs/userguide.html for a complete reference!

set $mod Mod4

# Font for window titles. Will also be used by the bar unless a different font
# is used in the bar {} block below.
font pango:Overpass 10
#font pango:monospace 8

# This font is widely installed, provides lots of unicode glyphs, right-to-left
# text rendering and scalability on retina/hidpi displays (thanks to pango).
#font pango:DejaVu Sans Mono 8

# Before i3 v4.8, we used to recommend this one as the default:
# font -misc-fixed-medium-r-normal--13-120-75-75-C-70-iso10646-1
# The font above is very space-efficient, that is, it looks good, sharp and
# clear in small sizes. However, its unicode glyph coverage is limited, the old
# X core fonts rendering does not support right-to-left and this being a bitmap
# font, it doesn’t scale on retina/hidpi displays.

# Use Mouse+$mod to drag floating windows to their wanted position
floating_modifier $mod

# start a terminal
bindsym $mod+Return exec i3-sensible-terminal

# open nautilus
bindsym $mod+Shift+Return exec nautilus

# toggle between previous/current workspaces
bindsym $mod+Tab workspace back_and_forth

# rename workspace
#bindsym Ctrl+Shift+r exec i3-input -F 'rename workspace to "%s"' -P 'rename current workspace: '

# kill focused window
bindsym $mod+Shift+q kill

# start dmenu (a program launcher)
#bindsym $mod+d exec dmenu_run
bindsym $mod+d exec rofi -show run
# There also is the (new) i3-dmenu-desktop which only displays applications
# shipping a .desktop file. It is a wrapper around dmenu, so you need that
# installed.
# bindsym $mod+d exec --no-startup-id i3-dmenu-desktop

# change focus
focus_follows_mouse no
bindsym $mod+j focus left
bindsym $mod+k focus down
bindsym $mod+l focus up
bindsym $mod+ccedilla focus right

# alternatively, you can use the cursor keys:
bindsym $mod+Left focus left
bindsym $mod+Down focus down
bindsym $mod+Up focus up
bindsym $mod+Right focus right

# move focused window
bindsym $mod+Shift+j move left
bindsym $mod+Shift+k move down
bindsym $mod+Shift+l move up
bindsym $mod+Shift+ccedilla move right

# alternatively, you can use the cursor keys:
bindsym $mod+Shift+Left move left
bindsym $mod+Shift+Down move down
bindsym $mod+Shift+Up move up
bindsym $mod+Shift+Right move right

# split in horizontal orientation
bindsym $mod+h split h

# split in vertical orientation
bindsym $mod+v split v

# enter fullscreen mode for the focused container
bindsym $mod+f fullscreen toggle

# change container layout (stacked, tabbed, toggle split)
bindsym $mod+s layout stacking
bindsym $mod+w layout tabbed
bindsym $mod+e layout toggle split

# toggle tiling / floating
bindsym $mod+Shift+space floating toggle

# change focus between tiling / floating windows
bindsym $mod+space focus mode_toggle

# focus the parent container
bindsym $mod+a focus parent

# focus the child container
#bindsym $mod+d focus child


#set $workspace1 "1:WWW"
# Name the workspaces
set $space1 " 1 ⏐ focus "
set $space2 " 2 ⏐ webs "
set $space3 " 3 ⏐ terms "
set $space4 " 4 "
set $space5 " 5 ⏐ parallel"
set $space6 " 6 "
set $space7 " 7 "
set $space8 " 8 ⏐ monitor"
set $space9 " 9 "
set $space10 " 10 "


# switch to workspace
bindsym $mod+1 workspace 1
bindsym $mod+2 workspace 2
bindsym $mod+3 workspace 3
bindsym $mod+4 workspace 4
bindsym $mod+5 workspace 5
bindsym $mod+6 workspace 6
bindsym $mod+7 workspace 7
bindsym $mod+8 workspace 8
bindsym $mod+9 workspace 9
bindsym $mod+0 workspace 10

# move focused container to workspace
bindsym $mod+Shift+1 move container to workspace 1
bindsym $mod+Shift+2 move container to workspace 2
bindsym $mod+Shift+3 move container to workspace 3
bindsym $mod+Shift+4 move container to workspace 4
bindsym $mod+Shift+5 move container to workspace 5
bindsym $mod+Shift+6 move container to workspace 6
bindsym $mod+Shift+7 move container to workspace 7
bindsym $mod+Shift+8 move container to workspace 8
bindsym $mod+Shift+9 move container to workspace 9
bindsym $mod+Shift+0 move container to workspace 10

#xprop + click on app -> search for the class
#assign [class="vlc"] $workspace1



# reload the configuration file
bindsym $mod+Shift+c reload
# restart i3 inplace (preserves your layout/session, can be used to upgrade i3)
bindsym $mod+Shift+r restart
# exit i3 (logs you out of your X session)
bindsym $mod+Shift+e exec "i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3? This will end your X session.' -b 'Yes, exit i3' 'i3-msg exit'"

# resize window (you can also use the mouse for that)
mode "resize" {
        # These bindings trigger as soon as you enter the resize mode

        # Pressing left will shrink the window’s width.
        # Pressing right will grow the window’s width.
        # Pressing up will shrink the window’s height.
        # Pressing down will grow the window’s height.
        bindsym j resize shrink width 5 px or 5 ppt
        bindsym k resize grow height 5 px or 5 ppt
        bindsym l resize shrink height 5 px or 5 ppt
        bindsym ccedilla resize grow width 5 px or 5 ppt

        # same bindings, but for the arrow keys
        bindsym Left resize shrink width 5 px or 5 ppt
        bindsym Down resize grow height 5 px or 5 ppt
        bindsym Up resize shrink height 5 px or 5 ppt
        bindsym Right resize grow width 5 px or 5 ppt

        # back to normal: Enter or Escape
        bindsym Return mode "default"
        bindsym Escape mode "default"
}

bindsym $mod+r mode "resize"


# set $bg-color			#000000
# set $inactive-bg-color		#000000
# set $text-color			#f3f4f5
# set $inactive-text-color	#676E7D
# set $urgent-bg-color		#44BBFF

# Color variables
set $color1 #38acd6
set $color2 #52938e
set $color3 #56b7b0
set $darklight #074860
set $highlight #3fc0ef
set $background #203e4a
set $background2 #5a8683



# client.focused			$bg-color		$bg-color		$text-color			#00FF00
# client.unfocused		$inactive-bg-color	$inactive-bg-color	$inactive-text-color		#00FF00
# client.focused_inactive		$inactive-bg-color	$inactive-bg-color	$inactive-text-color		#00FF00
# client.urgent			$urgent-bg-color	$urgent-bg-color	$text-color			#00FF00

hide_edge_borders both


# # Start i3bar to display a workspace bar (plus the system information i3status
# # finds out, if available)
# bar {
#         status_command i3status

# 	colors {
# 		background $bg-color
# 		statusline #44BBFF
# 		separator #757575
# 		focused_workspace	$bg-color		$bg-color		$text-color
# 		inactive_workspace	$inactive-bg-color	$inactive-bg-color	$inactive-text-color
# 		urgent_workspace	$urgent-bg-color	$urgent-bg-color	$text-color
# 		}
# 	tray_output primary


# }


# Start i3bar to display a workspace bar (plus the system information i3status finds out, if available)
bar {
        status_command i3status

        #tray_output primary

	colors {
        	#color class       border  back.   text
       		focused_workspace  $color1 $background $highlight
        	active_workspace   #555555 #555555 #ffffff
        	inactive_workspace $color2 #000000 $color2
        	urgent_workspace   $color1 $background2  $background

             statusline $color2
             separator $color2
    	}
}


#colors:                            border      backgr.     text            indicator
#client.focused                  $color3   $color3     $darklight  $color3
client.focused                    $background $background $color2  $color3
client.focused_inactive     	  #000000     #000000     $color2  $background
client.unfocused                  #000000     #000000     $color2  $background
client.urgent                     $color1     $background2 #ffffff $color1



exec --no-startup-id dropbox start
exec --no-startup-id nm-applet start
exec_always feh --bg-scale ~/Downloads/wall/wall.jpg

#Avoids touchpad misclicks while typing
exec syndaemon -i 0.5 -d -K

# Pulse Audio controls
bindsym XF86AudioRaiseVolume exec --no-startup-id pactl set-sink-volume 0 +5% #increase sound volume
bindsym XF86AudioLowerVolume exec --no-startup-id pactl set-sink-volume 0 -5% #decrease sound volume
bindsym XF86AudioMute exec --no-startup-id pactl set-sink-mute 0 toggle # mute sound

# Screen brightness controls
bindsym XF86MonBrightnessUp exec /usr/bin/increasekbdlight.sh
bindsym XF86MonBrightnessDown exec /usr/bin/decreasekbdlight.sh

# Touchpad controls
bindsym $mod+t exec toggletouchpad # toggle touchpad

# Screen update
bindsym $mod+m exec --no-startup-id /home/fpereira/.config/i3/screen_update.sh

# Media player controls
bindsym XF86AudioPlay exec playerctl play
bindsym XF86AudioPause exec playerctl pause
bindsym XF86AudioNext exec playerctl next
bindsym XF86AudioPrev exec playerctl previous


#i3 lock on black
bindsym $mod+shift+x exec i3lock -c 000000


#Suspend
#bindsym $mod+shift+s exec systemctl suspend
