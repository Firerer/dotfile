--
--[[
  lua cheatsheet: https://devhints.io/lua
  NOTE: order matters, latter modules may depends on earlier modules
]]
_G.prequire = function(m)
  local mod_exist, mod_or_err = pcall(require, m)
  if not mod_exist then return nil, mod_or_err end
  return mod_or_err
end

-- global options
require "di.options"
require "di.autocommands"

require "di.plugins"
require "di.keymaps"
