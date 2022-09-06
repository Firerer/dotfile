fish_hybrid_key_bindings

abbr --add wifi nmcli device wifi
abbr --add sfish source ~/.config/fish/config.fish

abbr --add cp "cp -i" # confirm before overwriting something
abbr --add df 'df -h' # human-readable sizes
abbr --add free 'free -h' # show sizes in MB
abbr --add rr 'ranger'
abbr --add rustr 'evcxr'                
abbr --add lr 'xplr' # like lf, it easy

# replace ls with exa
if type -q exa;
  alias ls "exa --icons --sort=type --group-directories-first "
  abbr --add lt 'ls -T --level 3' # tree listing
else
    abbr --add lt tree
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

# keybind
bind \cR -M insert "source ~/.config/fish/config.fish"; and commandline -f repaint-mode;
bind \co -M insert config_dotfile

set -Ux EDITOR nvim
set -Ux PYTHONPATH "."

fish_add_path -p ~/.bin ~/.local/bin ~/.cargo/bin ~/.emacs.d/bin \
  ~/Android/Sdk/build-tools/30.0.3 /home/linuxbrew/.linuxbrew/opt/tomcat@9/bin \

zoxide init fish | source
