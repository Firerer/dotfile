local group = vim.api.nvim_create_augroup("ConfigAutoLoad", { clear = true })

-- | reloading configs|
-- vim.fn.confirm
vim.api.nvim_create_autocmd("BufWritePost", {
  pattern = "leftwm/config.toml",
  callback = function()
    print "config.toml check:"
    vim.cmd "echo 'asdfasf'"
    -- !leftwm-check
  end,
  group = group,
})

vim.api.nvim_create_autocmd("BufWritePost", {
  pattern = "nvim/init.lua",
  command = "source <afile>| echo 'nvim init.lua reload'",
  group = group,
})

vim.api.nvim_create_autocmd("BufWritePost", {
  pattern = "*/zellij/config.yaml",
  callback = function()
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
vim.api.nvim_create_autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank { higroup = "Search", timeout = 100 }
  end
})

vim.api.nvim_create_autocmd({ "FileType" }, {
  pattern = {
    "Jaq",
    "qf",
    "help",
    "man",
    "lspinfo",
    "spectre_panel",
    "lir",
    "DressingSelect",
    "tsplayground",
  },
  callback = function()
    vim.cmd [[
      nnoremap <silent> <buffer> q :close<CR>
      set nobuflisted
    ]]
  end,
})

--
-- vim.api.nvim_create_augroup("lsp_format_on_save", {})
-- vim.api.nvim_create_autocmd("BufWritePre", {
--   group = "lsp_format_on_save",
--   pattern = opts.pattern,
--   callback = function()
--     require("lvim.lsp.utils").format {
--       timeout_ms = opts.timeout, filter = opts.filter
--     }
--   end,
-- })
