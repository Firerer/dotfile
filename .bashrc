# If not running interactively, don't do anything

[[ $- != *i* ]] && return

### enviroment ###
export USER_SHELL='fish'
export EDITOR='nvim'
export ALTERNATE_EDITOR="nano"                        # setting for emacsclient
export VISUAL="emacsclient -c -a emacs"           # $VISUAL use Emacs in GUI mode
export XDG_CONFIG_DIR="$HOME/.config"
export XDG_CONFIG_HOME="$HOME/.config"

# Compilation flags
export ARCHFLAGS="-arch x86_64"

# add working dir to PYTHONPATH
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

### ALIASES ###

# vim and emacs
alias cp="cp -i"                          # confirm before overwriting something
alias df='df -h'                          # human-readable sizes
alias free='free -h'                      # show sizes in MB
alias rustrepl='evcxr'                

# Changing "ls" to "exa"
if command -v exa &> /dev/null;
then
  alias ls='exa --color=always --group-directories-first --icons' # my preferred listing
  alias la='exa -a --color=always --group-directories-first --icons'  # all files and dirs
  alias ll='exa -la --color=always --group-directories-first --icons'  # long format
  alias lt='exa -aT --color=always --group-directories-first --icons' # tree listing
  alias l.='exa -a --icons| egrep "^\."'
else
  alias la='ls -a --color=always --group-directories-first --icons'  # all files and dirs
  alias ll='ls -la --color=always --group-directories-first --icons'  # long format
  alias lt='ls -aT --color=always --group-directories-first --icons' # tree listing
  alias l.='ls -a --icons| egrep "^\."'
fi

### ex - archive extractor ###
# # usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1     ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

### PROMPT ###
if command -v starship &> /dev/null
then
  shell=$(ps -p $$ -o comm=)
  source <(starship init $shell --print-full-init)
else
  PS1='[\u@\h \W]\$ '
fi
