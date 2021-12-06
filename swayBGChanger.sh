#!/bin/sh

PATH=$bash/bin:$swaybg/bin:$PATH

# swaybg -i $(find $1 -type f | shuf -n1) -m fill &
# OLD_PID=$!
# while true; do
#     sleep 600
#     swaybg -i $(find $1. -type f | shuf -n1) -m fill &
#     NEXT_PID=$!
#     sleep 5
#     kill $OLD_PID
#     OLD_PID=$NEXT_PID
# done

while true; do
    PID=`pidof swaybg`
    swaybg -i $(find $1. -type f | shuf -n1) -m fill &
    sleep 1
    kill PID
    sleep 599
done
