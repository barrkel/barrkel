#!/bin/bash (not really)

echo "* Initializing profile..."

# ....

export BARRKEL_HOME="${BARRKEL_HOME:-/barrkel}"
. "$BARRKEL_HOME"/base/all/bin/_debug
. "$BARRKEL_HOME"/common

clear

cat "$BARRKEL_HOME/banner"

echo "* Selected atoms: ${BARRKEL_ATOMS[@]}"

if [ -x /usr/bin/tput ]; then
    term_width=$(tput cols)
else
    term_width=80
fi

# Setup PATH
printf '* Paths\n'

ensure_prepending PATH /usr/bin /usr/X11R6/bin
test -d $GNU_BIN &&
    ensure_prepending PATH $GNU_BIN
ensure_prepending PATH /usr/local/bin /opt/bin
for atom in "${BARRKEL_ATOMS_REV[@]}"; do
    test -d "$BARRKEL_HOME"/base/"$atom"/bin &&
        ensure_prepending PATH "$BARRKEL_HOME"/base/"$atom"/bin
done
ensure_prepending PATH ~/bin

ensure_prepending MANPATH /usr/local/man /opt/man /usr/share/man /usr/man
ensure_prepending INFOPATH /usr/local/info /opt/info /usr/share/info /usr/info
append_if_missing PATH /usr/games

export PATH

# Prompt
__ Setting prompt

export HOSTNAME=$(hostname)
# Only print hostname in prompt if SSH connection
if [ -n "$SSH_CONNECTION" ]; then
    export PS1='[\h] \w\$ '
else
    export PS1='\w\$ '
fi

# Startup as appropriate
__ Running startup scripts

declare -a profile_dirs

printf '* Loading profile dirs\n'

for atom in "${BARRKEL_ATOMS_REV[@]}"; do
    test -d "$BARRKEL_HOME"/base/"$atom"/etc/profile.d &&
        array_add profile_dirs "$BARRKEL_HOME"/base/"$atom"/etc/profile.d
done

name_part=$((term_width / 10 * 3))
path_part=$((term_width - name_part - 5))
format_str="- %-$name_part"s" [%-$path_part"s']\n'

for d in "${profile_dirs[@]}"; do
    while read -r f; do
        if [ -f "$f" ]; then
            __ Running "$f"
            printf -- "$format_str" "$(basename "$f")" "$f"
            . "$f"
        fi
    done <<EOF
$($find -L "$d" -type f -name '*.sh' -o -name '*.login' | LC_ALL=C sort)
EOF
done

unset profile_dirs atom f d var item name_part path_part format_str term_width

printf '* Ready - press return...'
read
clear

stty-sane

[ -x "$(which fortune 2>/dev/null)" ] && fortune -a

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
