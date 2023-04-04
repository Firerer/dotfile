--
--[[
  lua cheatsheet: https://devhints.io/lua
  NOTE: order matters, latter modules may depends on earlier modules
]]
_G.saferequire = function(m)
  local mod_exist, mod_or_err = pcall(require, m)
  if not mod_exist then
    return nil, mod_or_err
  else
    return mod_or_err
  end
end

vim.cmd [[
  let fcitx5state=system("fcitx5-remote")
  " Disable the input method when exiting insert mode and save the state
  autocmd InsertLeave * :silent let fcitx5state=system("fcitx5-remote")[0] | silent !fcitx5-remote -c 
  " 2 means that the input method was opened in the previous state, and the input method is started when entering the insert mode
  autocmd InsertEnter * :silent if fcitx5state == 2 | call system("fcitx5-remote -o") | endif 
]]
-- global options
require "di.options"
require "di.autocommands"

require "di.plugins"
require "di.keymaps"
