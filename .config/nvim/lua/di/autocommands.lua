local group = vim.api.nvim_create_augroup("ConfigAutoLoad", { clear = true })

-- q to exits some buffer
vim.api.nvim_create_autocmd({ "FileType" }, {
  pattern = {
    "git",
    "fugitive",
    "DressingSelect",
    "Jaq",
    "help",
    "lir",
    "lspinfo",
    "man",
    "notify",
    "qf",
    "spectre_panel",
    "startuptime",
    "tsplayground",
  },
  callback = function()
    vim.cmd [[
      nnoremap <silent> <buffer> q :close<CR>
      set nobuflisted
    ]]
  end,
})

-- | reloading configs|
-- vim.fn.confirm
vim.api.nvim_create_autocmd("BufWritePost", {
  pattern = "leftwm/config.toml",
  callback = function()
    print "config.toml check:"
    vim.fn.system "leftwm-check"
  end,
  group = group,
})

vim.api.nvim_create_autocmd("BufWritePost", {
  pattern = "nvim/init.lua",
  command = "source <afile>| echo 'nvim init.lua reload'",
  group = group,
})

vim.api.nvim_create_autocmd("BufWritePost", {
  pattern = "*/zellij/config.kdl",
  callback = function()
    print "zellij config check:"
    local out = vim.fn.system(vim.fn.split "zellij setup --check")
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
  callback = function() vim.highlight.on_yank { higroup = "Search", timeout = 100 } end,
})

-- Check if we need to reload the file when it changed
vim.api.nvim_create_autocmd({ "FocusGained", "TermClose", "TermLeave" }, {
  command = "checktime",
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = { "gitcommit", "markdown" },
  callback = function()
    vim.opt_local.wrap = true
    vim.opt_local.spell = true
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

-- resize splits if window got resized
vim.api.nvim_create_autocmd({ "VimResized" }, {
  callback = function() vim.cmd "tabdo wincmd =" end,
})

vim.cmd [[
  augroup templates
    autocmd!
    autocmd BufNewFile *.sh 0r ~/.config/nvim/templates/shell.sh
    autocmd BufNewFile *.html 0r ~/.config/nvim/templates/website.html
  augroup END
]]

vim.cmd [[
  let fcitx5state=system("fcitx5-remote")
  " Disable the input method when exiting insert mode and save the state
  autocmd InsertLeave * :silent let fcitx5state=system("fcitx5-remote")[0] | silent !fcitx5-remote -c 
  " 2 means that the input method was opened in the previous state, and the input method is started when entering the insert mode
  autocmd InsertEnter * :silent if fcitx5state == 2 | call system("fcitx5-remote -o") | endif 
]]
