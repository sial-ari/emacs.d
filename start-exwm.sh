#!/bin/sh

if [ $# -eq 0 ]; then
    emacs --daemon --eval "(require 'exwm)" -f exwm-enable
    exec dbus-launch --exit-with-session emacsclient -c
fi

