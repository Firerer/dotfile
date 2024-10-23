[[ $- != *i* ]] && return # If not running interactively, do nothing

### enviroment ###
# add working dir to PYTHONPATH
export PYTHONPATH="."
# recommnaded by `man gpg-agent`
GPG_TTY=$(tty)
export GPG_TTY

### ALIASES ###
# vim and emacs
alias cp="cp -i"                          # confirm before overwriting something
alias df='df -h'                          # human-readable sizes
alias free='free -h'                      # show sizes in MB
alias rustrepl='evcxr'

# Changing "ls" to "exa"
if command -v exa &> /dev/null
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

### PROMPT ###
if command -v starship &> /dev/null
then
  shell=$(ps -p $$ -o comm=)
  source <(starship init "$shell" --print-full-init)
else
  PS1='[\u@\h \W]\$ '
fi
