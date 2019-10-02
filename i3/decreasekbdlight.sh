#!/bin/bash

bl=`cat /sys/class/backlight/intel_backlight/brightness`
if [ $bl -gt 0 ]
	then
	bl=`echo "$bl-500" | bc`
	echo $bl | sudo tee /sys/class/backlight/intel_backlight/brightness
fi
