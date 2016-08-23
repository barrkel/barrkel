xmodmap ~/.Xmodmap
xset fp+ ~/.fonts
xrdb ~/.Xdefaults > /dev/null 2>&1
pgrep syndaemon >/dev/null || syndaemon -i 0.5 -kRd

pgrep xscreensaver >/dev/null || (
    while true; do
        xscreensaver
    done &
)

