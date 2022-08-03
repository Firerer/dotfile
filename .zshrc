export ZSH="/home/di/.oh-my-zsh"
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

zstyle ':omz:update' mode reminder  # just remind me to update when it's time
ENABLE_CORRECTION="true"

# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
plugins=(
    tmux
    adb
    git
    sudo
    themes
    dotenv
    man
    # vi-mode
    # custom
    zsh-autosuggestions
    zsh-syntax-highlighting
    zsh-history-substring-search
)

source $ZSH/oh-my-zsh.sh
#tmux plugin
ZSH_TMUX_AUTOSTART="true"

# User configuration
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='nvim'
fi

# Compilation flags
export ARCHFLAGS="-arch x86_64"

export ALTERNATE_EDITOR=""                        # setting for emacsclient
export VISUAL="emacsclient -c -a emacs"           # $VISUAL use Emacs in GUI mode
export XDG_CONFIG_DIR="$HOME/.config"
export XDG_CONFIG_HOME="$HOME/.config"

# add current folder to PYTHONPATH
export PYTHONPATH="."

function addpath(){
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
    "$HOME/Android/Sdk/build-tools/30.0.3" # android SDK tools

### ALIASES ###

# vim and emacs
alias em="/usr/bin/emacs -nw"
alias emacs="emacsclient -c -a 'emacs'"
alias cp="cp -i"                          # confirm before overwriting something
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB
alias rustrepl='evcxr'                      # show sizes in MB

# Changing "ls" to "exa"
if command -v exa &> /dev/null
then
  alias ls='exa --color=always --group-directories-first --icons' # my preferred listing
  alias la='exa -a --color=always --group-directories-first --icons'  # all files and dirs
  alias ll='exa -hla --color=always --group-directories-first --icons'  # long format
  alias lt='exa -aT -L3 --color=always --group-directories-first --icons' # tree listing
  alias l.='ll | rg "\.\w+"' # only hidden files
fi

bindkey -s '^[z' 'fg ^M'
bindkey -s '^o' 'nvim $(fzf)^M'
# bindkey -v
# export KEYTIMEOUT=1

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
# __conda_setup="$('/home/di/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
# if [ $? -eq 0 ]; then
    # eval "$__conda_setup"
# else
    if [ -f "/home/di/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/home/di/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/di/anaconda3/bin:$PATH"
    fi
# fi

# unset __conda_setup
# <<< conda initialize <<<


# prompt
source <(/usr/bin/starship init zsh --print-full-init)

