#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# default command 
alias ls="ls --color=auto"
alias la="ls -a --color=auto"
alias ll="ls -al --color=auto"
#alias rm="rm -iv"

PS1='[\u@\h \W]\$ '

# common command renaming
#alias update="sudo pacman -Syu"
#alias install="sudo pacman -S "
#alias search="pacman -Ss "
alias sysinfo="neofetch"
 alias audio="pavucontrol &"

# exports
export XDG_CONFIG_DIR="$HOME/.config"
export XDG_CONFIG_HOME="$HOME/.config"
export PATH="$PATH:$HOME/bin"
alias config='/usr/bin/git --git-dir=/home/firer/.cfg/ --work-tree=/home/firer'
