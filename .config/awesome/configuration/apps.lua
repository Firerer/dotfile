local dpi = require("beautiful.xresources").apply_dpi
local menubar = require("menubar")
local awful = require "awful"

apps = {

  -- Your default terminal
  terminal        = "alacritty",

  -- Your default text editor
  editor          = os.getenv("EDITOR") or "vim",

  -- Your default file explorer
  explorer        = "pcmanfm",

}
awful.util.terminal = apps.terminal
apps.editor_cmd   = apps.terminal .. " -e " .. apps.editor
apps.explorer_cmd = apps.terminal .. " -e " .. apps.explorer
menubar.utils.terminal = apps.terminal -- Set the terminal for applications that require it


return apps
