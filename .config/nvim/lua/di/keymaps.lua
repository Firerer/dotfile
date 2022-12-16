local opts = { noremap = true, silent = true }
local mode_adapters = {
  insert_mode = "i",
  normal_mode = "n",
  term_mode = "t",
  visual_mode = "v",
  visual_block_mode = "x",
  command_mode = "c",
}

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
wk.setup()
vim.opt.timeoutlen = 300 -- faster cmp
wk.register {
  -- swap 0, ^
  ["0"] = { "^", "line first non blank" },
  ["^"] = { "0", "line first" },

  ["H"] = { ":bp<cr>", "prev buf" },
  ["L"] = { ":bn<cr>", "next buf" },

  ["<C-s>"] = { ":w<cr>", "save file" },

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

-- lsp related
local telefuncs = require "telescope.builtin"
wk.register({
  K = { vim.lsp.buf.hover, "hover" },
  g = {
    name = "goto",
    d = { telefuncs.lsp_definitions, "definition" },
    D = { vim.lsp.buf.declaration, "declaration" },
    i = { vim.lsp.buf.implementation, "implementation" },
    r = { telefuncs.lsp_references, "references" },
  },
  ["["] = { d = { vim.diagnostic.goto_prev, "prev error" } },
  ["]"] = { d = { vim.diagnostic.goto_next, "next error" } },
  ["<leader>"] = {
    l = {
      name = "lsp",
      e = { vim.diagnostic.open_float, "show error" },
      q = { vim.diagnostic.setlocalist, "show all errors" },
      i = { ":LspInfo<cr>", "lsp info" },
      D = { vim.lsp.buf.type_definition, "type definition" },
      a = { vim.lsp.buf.code_action, "code action" },
      f = { function() vim.lsp.buf.format { async = true } end, "format" },
      r = { vim.lsp.buf.rename, "rename" },
    },
    w = {
      name = "workspace",
      a = { vim.lsp.buf.add_workspace_folder, "add folder" },
      r = { vim.lsp.buf.remove_workspace_folder, "remove folder" },
      l = {
        function()
          print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
        end,
        "list folders",
      },
    },
  },
}, { buffer = bufnr })

wk.register({
  -- Better indenting
  ["<"] = "<gv",
  [">"] = ">gv",
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
  ["<C-j>"] = { 'pumvisible() ? "\\<C-n>" : "\\<C-j>"', { expr = true, noremap = true } },
  ["<C-k>"] = { 'pumvisible() ? "\\<C-p>" : "\\<C-k>"', { expr = true, noremap = true } },
}, { mode = "c", expr = true, noremap = true })
