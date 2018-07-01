#!/bin/bash

compton -b &
hsetroot -solid '#000077'
stalonetray &
setxkbmap -layout "us" -variant "dvorak" -option ctrl:nocaps
exec xmonad
