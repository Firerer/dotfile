local awful = require("awful")

local tagnames = {"1", "2", "3", "4", "5", "6", "7", "8", "9"}
-- awful.util.tagnames = { "🯱", "🯲", "🯳", "🯴", "🯵", "🯶", "🯷", "🯸", "🯹" }
local s = awful.layout.suit
local layouts_by_tag = {
    ["1"]=s.tile,
    ["2"]=s.tile,
    ["3"]=s.tile,
    ["4"]=s.fair,
    ["5"]=s.tile,
    ["6"]=s.tile,
    ["7"]=s.tile,
    ["8"]=s.tile,
    ["9"]=s.floating
}

local layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.max,
    awful.layout.suit.floating,
    awful.layout.suit.fair,
    --awful.layout.suit.tile.left,
    --awful.layout.suit.tile.bottom,
    --awful.layout.suit.tile.top,
    --awful.layout.suit.fair.horizontal,
    --awful.layout.suit.spiral,
    --awful.layout.suit.spiral.dwindle,
    --awful.layout.suit.max.fullscreen,
    --awful.layout.suit.magnifier,
    -- awful.layout.suit.corner.nw,
    -- awful.layout.suit.corner.ne,
    -- awful.layout.suit.corner.sw,
    -- awful.layout.suit.corner.se,
    -- lain.layout.cascade,
    -- lain.layout.cascade.tile,
    -- lain.layout.centerwork,
    -- lain.layout.centerwork.horizontal,
    -- lain.layout.termfair,
    -- lain.layout.termfair.center
}

awful.util.tagnames = tagnames
awful.util.layouts = layouts

awful.screen.connect_for_each_screen(function(s)
    awful.tag(tagnames, s, layouts[1])
    s.padding = { left = 0, right = 0, top = 0, buttom = 0 }
end)
