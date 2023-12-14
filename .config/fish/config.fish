abbr --add cp "cp -i" # confirm before overwriting something
abbr --add df 'df -h' # human-readable sizes
abbr --add free 'free -h' # show sizes in MB
# sed strip color, example `ssh | teelog`
abbr --add teelog 'sed -r "s/\x1B\[([0-9]{1,3}(;[0-9]{1,3})?)?[mGK]//g" | tee'
abbr --add rmm 'rm'
abbr --add mg 'magit.sh' # .bin/magit.sh
abbr --add sds 'systemd status '
abbr --add np 'nix profile'
abbr --add nf 'nix flake'
abbr --add nixos 'nixos-rebuild'
abbr --add hotspot 'nmcli device wifi list --rescan yes && nmcli device wifi connect '
type -q feh && abbr --add feh 'feh --scale-down'
type -q feh && abbr --add fehh 'feh'


type -q trash && abbr --add rm 'trash'
type -q trash && abbr --add rmm 'rm'
type -q dust && abbr --add du 'dust'

if type -q zellij;
  abbr --add zl 'zellij list-sessions'
  abbr --add z. "zellij --session (basename (pwd))"
  abbr --add za "zellij attach "
end

if type -q fzf;
  if type -q zoxide;
    abbr --add zd 'cd (zoxide query -l | fzf)'
  end
  abbr --add zdd 'cd (fd --type directory -u | fzf)'
end

# replace ls with exa
if type -q exa;
  alias ls "exa --icons --sort=type --group-directories-first "
  abbr --add lt 'ls -T --level 3' # tree listing
else
    type -q tree && abbr --add lt 'tree -L 3'
end
abbr --add la "ls -a"
abbr --add ll "ls -hla"
abbr --add l. 'ls -a | rg "\.\w*"'


if type -q nvim;
  set -Ux EDITOR 'nvim'
  set -Ux VISUAL 'nvim'
end
type -q code && set -Ux VISUAL 'code --wait'

fish_add_path -p ~/.bin \
  ~/.local/bin \
  ~/.cargo/bin \
  ~/Applications
set -Ux PYTHONPATH "."

zoxide init fish | source
source (/usr/bin/starship init fish --print-full-init | psub)
fd --gen-completions fish | source

source ~/.config/fish/keybind.fish

