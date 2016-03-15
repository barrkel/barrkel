xmodmap ~/.Xmodmap
xset fp+ ~/.fonts
xrdb ~/.Xdefaults
pgrep syndaemon || syndaemon -i 0.5 -KRd
