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
source .bashrc

#tmux plugin
ZSH_TMUX_AUTOSTART="true"


bindkey -s '^[z' 'fg ^M'
bindkey -s '^o' 'nvim $(fzf)^M'
