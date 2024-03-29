export USER_SHELL='fish'

addpath() {
    for var in "$@"
    do
        if [ -d "$var" ] ;
            then PATH="$var:$PATH"
        fi
    done
}

### PATH
addpath "$HOME/.bin" \
    "$HOME/.local/bin" \
    "$HOME/.emacs.d/bin" \
    "$HOME/.cargo/bin" \
    "$HOME/.nix-profile/bin/librewolf" \

if command -v nvim > /dev/null
then
    export EDITOR='nvim'
else
    export EDITOR='vim'
fi
export ALTERNATE_EDITOR="nano"                        # setting for emacsclient

if command -v code > /dev/null
then
  export VISUAL="nvim"
fi

# Compilation flags
export ARCHFLAGS="-arch x86_64"

# if [[ -z $DISPLAY ]] && [[ $(tty) = "/dev/tty1" ]];
# then
#   exec startx >> /tmp/xlog-$(date +%F-%T) 2>&1
# fi

eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
