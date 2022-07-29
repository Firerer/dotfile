local naughty = require("naughty")
local gears = require("gears")

function dbg(s)
    naughty.notify({text=s})
end

local c = client.focus

-- if c then
--     dbg("asdfasdf")
-- end

return gears.filesystem.get_themes_dir()
