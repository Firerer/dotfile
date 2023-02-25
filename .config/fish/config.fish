fish_hybrid_key_bindings

abbr --add cp "cp -i" # confirm before overwriting something
abbr --add df 'df -h' # human-readable sizes
abbr --add free 'free -h' # show sizes in MB
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
bind \co -M insert config_dotfile

set -Ux EDITOR 'nvim'
set -Ux PYTHONPATH "."
set -Ux TERMINAL "alacritty"

fish_add_path -p ~/.bin ~/.local/bin ~/.cargo/bin /opt/android-sdk/platform-tools/ ~/Applications

zoxide init fish | source

if type -q nnn;
  abbr --add nnn 'nnn'
  set -Ux LC_COLLATE "C" # hidden files on top
  set -Ux NNN_FIFO "/tmp/nnn.fifo" # temporary buffer for the previews
  set -Ux NNN_OPTS 'adeH'
  # https://github.com/jarun/nnn/tree/master/plugins
  # set -Ux NNN_PLUG_PERSONAL 'g:personal/convert2zoom;p:personal/echo'
  # NNN_PLUG_WORK='j:work/prettyjson;d:work/foobar'
  # NNN_PLUG_INLINE='e:!go run $nnn*'
  # NNN_PLUG_DEFAULT='1:ipinfo;p:preview-tui;o:fzz;b:nbak'
  # NNN_PLUG="$NNN_PLUG_PERSONAL;$NNN_PLUG_WORK;$NNN_PLUG_DEFAULT;$NNN_PLUG_INLINE"
  set -Ux NNN_PLUG 'p:preview-tabbed'
  set -Ux NNN_TRASH '1' # n=1: trash-cli, n=2: gio trash
  set -Ux NNN_TMPFILE '/tmp/.lastd' # always cd on quite
end

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/di/Applications/google-cloud-sdk/path.fish.inc' ]; . '/home/di/Applications/google-cloud-sdk/path.fish.inc'; end
