fish_hybrid_key_bindings

abbr --add cp "cp -i" # confirm before overwriting something
abbr --add df 'df -h' # human-readable sizes
abbr --add free 'free -h' # show sizes in MB
abbr --add teelog 'sed -r "s/\x1B\[([0-9]{1,3}(;[0-9]{1,3})?)?[mGK]//g" | tee' # tee no color
type -q trash && abbr --add rm 'trash'
abbr --add mg 'magit.sh' # .bin/magit.sh

# replace ls with exa
if type -q exa;
  alias ls "exa --icons --sort=type --group-directories-first "
  abbr --add lt 'ls -T --level 3' # tree listing
else
    abbr --add lt 'tree -L 3'
end

abbr --add la "ls -a"
abbr --add ll "ls -hla"
abbr --add l. 'ls -a | rg "\.\w*"'

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

fish_add_path -p ~/.bin \
  ~/.local/bin \
  ~/.cargo/bin \
  ~/Applications

zoxide init fish | source

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/di/Applications/google-cloud-sdk/path.fish.inc' ]; . '/home/di/Applications/google-cloud-sdk/path.fish.inc'; end

# tabtab source for packages
# uninstall by removing these lines
[ -f ~/.config/tabtab/fish/__tabtab.fish ]; and . ~/.config/tabtab/fish/__tabtab.fish; or true
eval (/home/linuxbrew/.linuxbrew/bin/brew shellenv)
