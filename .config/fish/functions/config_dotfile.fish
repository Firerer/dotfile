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
