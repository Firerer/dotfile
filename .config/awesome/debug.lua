local naughty = require("naughty")
local gears = require("gears")
local d = require("gears.debug")

function dbg(s)
    naughty.notify({text=s})
end

local c = client.focus
local gtk = require("beautiful.gtk")

d.dump(gtk, "debug")

-- if c then
--     dbg("asdfasdf")
-- end

return gears.filesystem.get_themes_dir()
