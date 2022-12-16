--[[
  lua cheatsheet: https://devhints.io/lua
  NOTE: order matters, latter modules may depends on earlier modules
]]

-- speed up config loading
local ok, impatient = pcall(require, 'impatient')
if ok then
  impatient.enable_profile()
else
  vim.notify(impatient)
end

-- global options
require "di.options"
require "di.autocommands"

require "di.plugins"
require "di.keymaps"
