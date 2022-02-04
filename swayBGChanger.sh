#!/bin/sh

PATH=$bash/bin:$swaybg/bin:$killall/bin:$PATH

while true; do
    if [[ $(pidof sway) == $(pidof nonexistent) ]]; then
	break
    else
	PID=`pidof swaybg`
	swaybg -i $(find $1 -type f | shuf -n1) -m fill &
	sleep 1
	kill $PID
	sleep 599
    fi
done
