#!/bin/sh

# Disable access control
xhost +SI:localuser:$USER

# Fallback cursor
xsetroot -cursor_name left_ptr

# Keyboard repeat rate
xset r rate 200 60

export VISUAL=emacsclient
export EDITOR="$VISUAL"

# Start Emacs
exec dbus-launch --exit-with-session emacs
