fish_hybrid_key_bindings

abbr --add cp "cp -i" # confirm before overwriting something
abbr --add df 'df -h' # human-readable sizes
abbr --add free 'free -h' # show sizes in MB
abbr --add mg 'magit.sh' # .bin/magit.sh
abbr --add teelog 'sed -r "s/\x1B\[([0-9]{1,3}(;[0-9]{1,3})?)?[mGK]//g" | tee' # tee no color
type -q evcxr && abbr --add rustr 'evcxr'
type -q trash && abbr --add rm 'trash'

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

source (/usr/bin/starship init fish --print-full-init | psub)

# keybind
bind \cR -M insert "source ~/.config/fish/config.fish"; and commandline -f repaint-mode;

function fzf_open
  set -f before $PWD
  set -fx FZF_DEFAULT_COMMAND 'fd -HIE .git'
  if test $PWD = $HOME
    cd ~/dotfile/
  end
  set -f target (fzf)
  if test $status -eq 0
    nvim $target
  end
  cd $before
end
bind \co -M insert fzf_open

set -Ux EDITOR 'nvim'
set -Ux VISUAL 'nvim'
set -Ux PYTHONPATH "."
set -gx TERM "xterm-256color"

fish_add_path -p ~/.bin ~/.local/bin ~/.cargo/bin /opt/android-sdk/platform-tools/ ~/Applications

zoxide init fish | source

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/di/Applications/google-cloud-sdk/path.fish.inc' ]; . '/home/di/Applications/google-cloud-sdk/path.fish.inc'; end
