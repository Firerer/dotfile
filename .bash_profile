export TERMINAL='/usr/bin/alacritty'
export TERM=$TERMINAL
export USER_SHELL='fish'

addpath(){
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


if command -v nvim > /dev/null
then
    export EDITOR='nvim'
else
    export EDITOR='vim'
fi
export ALTERNATE_EDITOR="nano"                        # setting for emacsclient

if command -v emacs > /dev/null
then
  export VISUAL="emacsclient -c -a emacs"           # $VISUAL use Emacs in GUI mode
fi

# Compilation flags
export ARCHFLAGS="-arch x86_64"

if [[ -z $DISPLAY ]] && [[ $(tty) = "/dev/tty1" ]];
then
  exec startx
fi
