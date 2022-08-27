local awful = require('awful')
local beautiful = require('beautiful')
local wibox = require('wibox')
local gears = require('gears')

configuration = require('configuration.config')
require('widgets.top-panel')

local TopPanel = function(s)
  -- wiboxes are much more flexible than wibars simply for the fact that there are no defaults,
  local panel = wibox({
        ontop = true,
        screen = s,
        height = configuration.toppanel_height,
        width = s.geometry.width,
        x = s.geometry.x,
        y = s.geometry.y,
        stretch = false,
        bg = beautiful.background,
        fg = beautiful.fg_normal,
        struts = {
          top = configuration.toppanel_height
        }
      }
    )

  panel:struts( {
      top = configuration.toppanel_height
  })
  --
  panel:setup {
    layout = wibox.layout.align.horizontal,
    expand = "none",
    { -- Left widgets
      layout = wibox.layout.fixed.horizontal,
      -- mylauncher,
      s.mylayoutbox,
      s.mytaglist,
      s.mypromptbox,
    },
     -- Middle widget
      s.mytasklist,
    { -- Right widgets
      layout = wibox.layout.fixed.horizontal,
      -- mykeyboardlayout,
      mytextclock,
      wibox.widget.systray(),
    },
  }


  return panel
end

return TopPanel

