local api = vim.api
local group = api.nvim_create_augroup("ConfigAutoLoad", { clear = true })

-- | reloading configs|
-- vim.fn.confirm
api.nvim_create_autocmd("BufWritePost", {
  pattern="leftwm/config.toml",
  callback = function()
    print "config.toml check:"
    vim.cmd "echo 'asdfasf'"
    -- !leftwm-check
  end,
  group = group,
})

api.nvim_create_autocmd("BufWritePost", {
  pattern = "nvim/init.lua",
  command = "source <afile>| echo 'nvim init.lua reload'",
  group = group,
})

api.nvim_create_autocmd("BufWritePost", {
  pattern = "*/zellij/config.yaml",
  callback = function ()
    print "zellij config check:"
    local out = vim.fn.system(vim.fn.split("zellij setup --check"))
    print(out)
  end,
  group = group,
})

-- vim.cmd [[
--   augroup packer_user_config
--     autocmd!
--     autocmd BufWritePost ~/.config/nvim/init.lua source <afile>
--   augroup end
-- ]]

-- | highlight |
api.nvim_create_autocmd("TextYankPost", {
  callback = function ()
    vim.highlight.on_yank{}
  end
})
