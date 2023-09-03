fish_hybrid_key_bindings
fzf_key_bindings

function reload_config
  source ~/.config/fish/config.fish
end
bind \cR -M insert reload_config; and commandline -f repaint-mode;

function fzf_open
  set -f before $PWD
  set -fx FZF_DEFAULT_COMMAND 'fd -HIE .git'
  if test $PWD = $HOME
    cd ~/dotfile/
  end
  set -f target (fzf)
  if test $status -eq 0
    xdg-open $target
  end
  cd $before
end
bind \co -M insert fzf_open
