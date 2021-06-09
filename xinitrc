setxkbmap -layout "us" -variant "dvorak" -option ctrl:nocaps
# xrandr --output DP-2 --right-of DP-1 --rotate right
xrandr --output DP-2 --right-of DP-4 --rotate right
inputplug -c bin/usb-switch
qtile
