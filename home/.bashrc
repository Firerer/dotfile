#
# ~/.bashrc
#


# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias cp='cp -i'
alias df='df -h'
alias free='free -h'

if command -v eza &>/dev/null; then
  alias ls='eza --color=always --group-directories-first --icons'
  alias la='eza -a --color=always --group-directories-first --icons'
  alias ll='eza -la --color=always --group-directories-first --icons'
  alias lt='eza -aT --color=always --group-directories-first --icons'
else
  alias la='ls -A --color=auto'
  alias ll='ls -Al --color=auto'
fi

if command -v starship &>/dev/null; then
  eval "$(starship init bash)"
else
  PS1='[\u@\h \W]\$ '
fi

export PATH="$HOME/.local/state/nix/profiles/dotfiles/bin:$HOME/.local/bin:$PATH"
export XDG_DATA_DIRS="$HOME/.local/state/nix/profiles/dotfiles/share:${XDG_DATA_DIRS:-/usr/local/share:/usr/share}"
export TERMINAL="alacritty"

if [ -d "$HOME/.xberg/bin" ]; then
  export PATH="$HOME/.xberg/bin:$PATH"
fi

# pnpm
export PNPM_HOME="$HOME/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME/bin:"*) ;;
  *) export PATH="$PNPM_HOME/bin:$PATH" ;;
esac
# pnpm end

# Replace rm with trash, use rmm for permanent deletion
alias rm='gio trash'
alias rmm='/usr/bin/rm'

command -v zoxide &>/dev/null && eval "$(zoxide init bash)"

export PYTHONPATH="${PYTHONPATH:+$PYTHONPATH:}."
export TERMINFO_DIRS="$HOME/.local/state/nix/profiles/dotfiles/share/terminfo:/usr/share/terminfo${TERMINFO_DIRS:+:$TERMINFO_DIRS}"
export GPG_TTY
GPG_TTY=$(tty)

if command -v nvim &>/dev/null; then
  export EDITOR=nvim
fi

if command -v nvm &>/dev/null; then
  export NVM_DIR="$HOME/.nvm"
  [[ -s "$NVM_DIR/nvm.sh" ]] && source "$NVM_DIR/nvm.sh"
  [[ -s "$NVM_DIR/bash_completion" ]] && source "$NVM_DIR/bash_completion"
fi


# >>> grok installer >>>
export PATH="$HOME/.grok/bin:$PATH"
[[ -r "$HOME/.grok/completions/bash/grok.bash" ]] && source "$HOME/.grok/completions/bash/grok.bash"
# <<< grok installer <<<


export PATH="$PATH:$HOME/.lmstudio/bin"
