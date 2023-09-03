function config_dotfile
  set -f before $PWD
  set -fx FZF_DEFAULT_COMMAND 'fd -HIE .git --type file'
  cd ~/dotfile/
  if test $status -eq 0
    open (fzf)
  end
  cd $before
end
