-- https://github.com/dtomvan/xpm.xplr#plugin-arguments
-- https://xplr.dev/en/awesome-plugins
---@diagnostic disable
local xplr = xplr -- The globally exposed configuration to be overridden.
---@diagnostic enable
local home = os.getenv("HOME")
local xpm_path = home .. "/.local/share/xplr/dtomvan/xpm.xplr"
local xpm_url = "https://github.com/dtomvan/xpm.xplr"

package.path = package.path .. ";" .. xpm_path .. "/?.lua;" .. xpm_path .. "/?/init.lua"
os.execute(string.format("[ -e '%s' ] || git clone '%s' '%s'", xpm_path, xpm_url, xpm_path))

require("xpm").setup({
	plugins = {
		"dtomvan/xpm.xplr",
		{
			"sayanarijit/alacritty.xplr",
			setup = function()
				require("alacritty").setup({
					mode = "default",
					key = "ctrl-n",
					send_focus = true,
					send_selection = true,
					extra_alacritty_args = "",
					extra_xplr_args = "",
				})
			end,
		},
		"Junker/nuke.xplr",
		"sayanarijit/map.xplr ",
		"sayanarijit/command-mode.xplr",
		{ name = "sayanarijit/fzf.xplr" },
	},
	auto_install = true,
	auto_cleanup = true,
})

require("command-mode").setup({
	mode = "default",
	key = ":",
	remap_action_mode_to = {
		mode = "default",
		key = ";",
	},
})

xplr.config.modes.builtin.default.key_bindings.on_key.x = {
	help = "xpm",
	messages = {
		"PopMode",
		{ SwitchModeCustom = "xpm" },
	},
}

-- require("map").setup({
-- 	mode = "default", -- or `xplr.config.modes.builtin.default`,
-- 	key = "M",
-- 	editor = os.getenv("EDITOR") or "vim",
-- 	editor_key = "ctrl-o",
-- 	prefer_multi_map = false,
-- 	placeholder = "{}",
-- 	custom_placeholders = {
-- 		["{ext}"] = function(node)
-- 			-- See https://xplr.dev/en/lua-function-calls#node
-- 			return node.extension
-- 		end,
--
-- 		["{name}"] = map.placeholders["{name}"],
-- 	},
-- })
