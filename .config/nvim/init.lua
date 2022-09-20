--[[
lua cheatsheet: https://devhints.io/lua

NOTE: order matters, latter modules may depends on earlier modules
]]

--NOTE: modules for shared resources
-- require "di.utils"

-- global options
require "di.autocommands"
require "di.options"

local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system { "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path }
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end

PACKER_BOOTSTRAP = ensure_packer()

local packer = require "packer"
packer.init {
  -- auto_clean = true,
  display = {
    open_fn = function()
      local result, win, buf = require("packer.util").float {
        border = {
          { "╭", "FloatBorder" },
          { "─", "FloatBorder" },
          { "╮", "FloatBorder" },
          { "│", "FloatBorder" },
          { "╯", "FloatBorder" },
          { "─", "FloatBorder" },
          { "╰", "FloatBorder" },
          { "│", "FloatBorder" },
        },
      }
      vim.api.nvim_win_set_option(win, "winhighlight", "NormalFloat:Normal")
      return result, win, buf
    end,
  },
  profile = {
    enable = true,
    threshold = 1,
  },
}

require "di.plugins"
require "di.keymaps"

if PACKER_BOOTSTRAP then
  require("packer").sync()
end
