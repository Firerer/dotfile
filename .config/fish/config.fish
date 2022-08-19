fish_hybrid_key_bindings

abbr --add wifi nmcli device wifi
abbr --add sfish source ~/.config/fish/config.fish

abbr --add cp "cp -i"                          # confirm before overwriting something
abbr --add df 'df -h'                          # human-readable sizes
abbr --add free 'free -h'                      # show sizes in MB
abbr --add rr 'ranger'
abbr --add rust 'evcxr'                

# replace ls with exa
if type -q exa;
  alias ls "exa --icons --sort=type --group-directories-first "
  abbr --add lt 'ls --icons --level 3' # tree listing
end

abbr --add la "ls -a"
abbr --add ll "ls -hla"
abbr --add l. 'ls -a | egrep "^\."'

if type -q tmux;
  abbr --add tl tmux list-session
  abbr --add ta tmux attach -t
  abbr --add ts tmux new-session -s
end

source (/usr/bin/starship init fish --print-full-init | psub)

function config_dotfile
  set -f before $PWD
  set -fx FZF_DEFAULT_COMMAND 'fd -HE .git'
  cd ~/dotfile/
  set -f target (fzf)
  if test $status -eq 0
    nvim $target
  end
  cd $before
end

# keybind
bind \cR -M "source ~/.config/fish/config.fish"; and commandline -f repaint-mode;
bind \co -M insert config_dotfile
