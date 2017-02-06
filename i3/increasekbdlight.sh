#!/bin/bash

bl=`cat /sys/class/backlight/intel_backlight/brightness`
mx=`cat /sys/class/backlight/intel_backlight/max_brightness`
bl=`echo "$bl+50" | bc` 
if [ $bl -lt $mx ]
	then
	echo $bl | sudo tee /sys/class/backlight/intel_backlight/brightness 2> /home/fpereira/lo 
fi
