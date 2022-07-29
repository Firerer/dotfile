local naughty       = require("naughty")

-- Autostart windowless processes
local function run_once(cmd_arr)
    for _, cmd in ipairs(cmd_arr) do
        local c = string.format("pgrep -u $USER -fx '%s' > /dev/null || (%s)", cmd, cmd)
        awesome.exec(
            string.format("pgrep -u $USER -fx '%s' > /dev/null || (%s)",
        cmd, cmd))
    end
end

run_once({ "emacs --daemon",
           -- "nitrogen --restore &",
           "feh --bg-fill --randomize ~/Pictures/wallpapers/",
           "~/.config/polybar/launch.sh",
           "nm-applet",
           "fcitx5 &"
}) 

-- This function implements the XDG autostart specification
--[[
awful.spawn.with_shell(
    'if (xrdb -query | grep -q "^awesome\\.started:\\s*true$"); then exit; fi;' ..
    'xrdb -merge <<< "awesome.started:true";' ..
    -- list each of your autostart commands, followed by ; inside single quotes, followed by ..
    'dex --environment Awesome --autostart --search-paths "$XDG_CONFIG_DIRS/autostart:$XDG_CONFIG_HOME/autostart"' -- https://github.com/jceb/dex
)
--]]


-- Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify {
        preset = naughty.config.presets.critical,
        title = "Oops, there were errors during startup!",
        text = awesome.startup_errors
    }
    require("rc.template") -- fallback
end

-- Handle runtime errors after startup
do
    local in_error = false

    awesome.connect_signal("debug::error", function (err)
        if in_error then return end

        in_error = true

        naughty.notify {
            preset = naughty.config.presets.critical,
            title = "Oops, an error happened!",
            text = tostring(err)
        }

        in_error = false
    end)
end
-- }}}
