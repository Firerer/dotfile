local opts = { noremap = true, silent = true }
vim.cmd [[ 
  nnoremap <silent> <esc> :noh<return><esc>
  nnoremap <silent> <esc>^[ <esc>^[
]]
vim.api.nvim_set_keymap("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

vim.api.nvim_set_keymap("i", "jk", "<ESC>", opts)

-- https://github.com/folke/which-key.nvim
-- NOTE: starts with "<Plug>", whichkey set noremap=false automatically
local wk = require "which-key"
wk.setup {}
vim.opt.timeoutlen = 300 -- faster cmp
wk.register {
  -- swap 0, ^
  ["0"] = { "^", "line first non blank" },
  ["^"] = { "0", "line first" },

  ["H"] = { ":bp<cr>", "prev buf" },
  ["L"] = { ":bn<cr>", "next buf" },

  ["<leader>"] = {
    ["<leader>"] = { ":find ", ":find" },
    f = {
      name = "file",
      Y = { [[ :call setreg('+', expand('%:~'))<cr> ]], "yank full path" },
      y = { [[ :call setreg('+', expand('%:t'))<cr> ]], "yank file name" },
    },
    h = {
      name = "help",
    },
    b = {
      name = "buffer",
      n = { ":ene<cr>", "new buffer" },
      d = { ":bd<cr>", "buffer delete" },
    },
    g = {
      name = "git",
    },
    s = {
      name = "search",
    },
    p = {
      name = "project",
    },
    q = {
      name = "quit",
      q = { ":q<cr>", "exit" },
      r = { ":source ~/.config/nvim/init.lua<cr>", "reload config" },
      c = { ":PackerCompile<cr>", "packer compile" },
      s = { ":PackerSync<cr>", "packer sync" },
    },
  },
}

wk.register({}, {
  mode = "v",
  prefix = "",
  buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
  silent = true, -- use `silent` when creating keymaps
  noremap = true, -- use `noremap` when creating keymaps
  nowait = false, -- use `nowait` when creating keymaps
})

wk.register({}, { mode = "i" })
