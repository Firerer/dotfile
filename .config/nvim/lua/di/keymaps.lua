local opts = { noremap = true, silent = true }
local mode_adapters = {
  insert_mode = "i",
  normal_mode = "n",
  term_mode = "t",
  visual_mode = "v",
  visual_block_mode = "x",
  command_mode = "c",
}

-- esc cancel search hl
vim.cmd [[
    nnoremap <silent> <esc> :noh<return><esc>
    nnoremap <silent> <esc>^[ <esc>^[
]]

-- setup leader key
vim.api.nvim_set_keymap("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

vim.api.nvim_set_keymap("i", "jk", "<ESC>", opts)

-- keep curser place when join lines
vim.api.nvim_set_keymap("n", "J", "mzJ`z", opts)
-- center curser when move
vim.api.nvim_set_keymap("n", "<C-d>", "<C-d>zz", opts)
vim.api.nvim_set_keymap("n", "<C-u>", "<C-u>zz", opts)
-- search term in middle
vim.api.nvim_set_keymap("n", "n", "nzzzv", opts)
vim.api.nvim_set_keymap("n", "N", "Nzzzv", opts)

local mark = require "harpoon.mark"
local ui = require "harpoon.ui"

vim.keymap.set("n", "<C-a>", mark.add_file)
vim.keymap.set("n", "<C-e>", ui.toggle_quick_menu)
-- vim.keymap.set("n", "<C-1>", function() ui.nav_file(1) end)
-- vim.keymap.set("n", "<C-2>", function() ui.nav_file(2) end)
-- vim.keymap.set("n", "<C-3>", function() ui.nav_file(3) end)
-- vim.keymap.set("n", "<C-4>", function() ui.nav_file(4) end)
-- https://github.com/folke/which-key.nvim
-- NOTE: starts with "<Plug>", whichkey set noremap=false automatically
local wk = require "which-key"
local telefuncs = prequire("telescope.builtin")
if not telefuncs then
  error("failed to load telefuncs")
end

wk.setup()
vim.opt.timeoutlen = 311 -- faster cmp
wk.register {
  -- swap 0, ^
  ["0"] = { "^", "line first non blank" },
  ["^"] = { "0", "line first" },

  ["H"] = { ":bp<cr>", "prev buf" },
  ["L"] = { ":bn<cr>", "next buf" },

  ["<C-s>"] = { ":w<cr>", "save file" },

  g = {
    h = { ":Telescope heading<cr>", "goto heading" },
  },
  ["<leader>"] = {
    ["<leader>"] = { function()
      if vim.fn.system("git rev-parse --git-dir 2> /dev/null") == "" then
        -- not in git repo
        telefuncs.fd()
      else
        telefuncs.git_files()
      end
    end, "find files" },
    d = { [["_d]], "no yank delete" },
    t = {
      name = "toogle",
      u = { ":UndotreeToggle<cr><C-w><C-w>", "undo tree" },
      c = { ":ColorizerToggle<cr>", "colorizer" },
      l = { ":Lazy<cr>", "Lazy.nvim" },
      m = { ":Mason<cr>", "Mason.nvim" },
    },
    f = {
      name = "file",
      e = { ":Ex<cr>", "file explore" },
      x = { "<cmd>!chmod +x %<cr>", "current file +x" },
      f = { function()
        telefuncs.fd({ hidden = true, no_ignore = true })
      end, "find all files" },
      r = { telefuncs.oldfiles, "recent files" },
      s = { telefuncs.current_buffer_fuzzy_find, "fuzzy search" },
      t = { telefuncs.tags, "find tags" },
      v = { telefuncs.treesitter, "treesitter vars" },
    },
    y = {
      name = "yank",
      A = { "ggVGy", "yank all file" },
      F = { [[ :call setreg('+', expand('%:~'))<cr> ]], "yank full path" },
      f = { [[ :call setreg('+', expand('%:t'))<cr> ]], "yank file name" },
    },
    p = {
      p = { require 'telescope'.extensions.project.project, "switch project", },
      s = { telefuncs.live_grep, "project search" },
    },
    h = {
      name = "help",
      b = { telefuncs.builtin, "telescope builtin" },
      h = { telefuncs.help_tags, "help tags" },
      k = { telefuncs.keymaps, "find keymaps" },
      t = { telefuncs.colorscheme, "find theme" },
      c = { telefuncs.commands, "commands" },
      s = { telefuncs.symbols, "symbols" },
      m = { telefuncs.man_pages, "man pages" },
    },
    b = {
      name = "buffer",
      n = { ":ene<cr>", "new buffer" },
      d = { ":bd<cr>", "buffer delete" },
    },
    g = {
      name = "git",
      b = { ":Git branch", "git branches" },
      g = { ":Git<cr>", "status" },
      c = { ":Git commit<cr>", "commit" },
      l = { ":Git log<cr>", "log" },
      a = { ":Git add %<cr>", "add current file" },
      A = { ":Git add .<cr>", "add all" },
    },
    s = {
      name = "search",
      r = { [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]], "replace sel word" },
    },
    q = {
      name = "quit",
      q = { ":q<cr>", "exit" },
      r = { ":source ~/.config/nvim/init.lua<cr>", "reload config" },
      u = { ":Lazy update<cr>", "lazy update" },
      s = { ":Lazy sync<cr>", "lazy sync" },
    },

  },
}


wk.register({
  -- Better indenting
  ["<"] = "<gv",
  [">"] = ">gv",
  -- move selected line
  J = ":m '>+1<CR>gv=gv",
  K = ":m '<-2<CR>gv=gv",

  -- preserve yanked text in register
  ["<leader>p"] = { "\"_dp", "no yank paste" }
}, {
  mode = "v",
  prefix = "",
  buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
  silent = true, -- use `silent` when creating keymaps
  noremap = true, -- use `noremap` when creating keymaps
  nowait = false, -- use `nowait` when creating keymaps
})

wk.register({
  -- navigate tab completion with <c-j> and <c-k>
  -- runs conditionally
  ["<C-j>"] = { 'pumvisible() ? "\\<C-n>" : "\\<C-j>"', },
  ["<C-k>"] = { 'pumvisible() ? "\\<C-p>" : "\\<C-k>"', },
}, { mode = "c", noremap = true })

wk.register({
  -- preserve yanked text in register
  ["<leader>p"] = { "\"_dp", "no yank paste" }
}, { mode = "x" })
