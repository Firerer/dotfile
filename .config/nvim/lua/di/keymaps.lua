local opts = { noremap = true, silent = true }
local insert_mode = "i"
local normal_mode = "n"
local term_mode = "t"
local visual_mode = "v"
local visual_block_mode = "x"
local command_mode = "c"

-- esc cancel search hl
vim.cmd [[
    nnoremap <silent> <esc> :noh<return><esc>
    nnoremap <silent> <esc>^[ <esc>^[
]]

-- setup leader key
vim.api.nvim_set_keymap("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "
vim.g.maplocalleader = " ;"

vim.api.nvim_set_keymap("i", "jk", "<ESC>", opts)

-- keep curser place when join lines
vim.api.nvim_set_keymap("n", "J", "mzJ`z", opts)
-- center curser when move
vim.api.nvim_set_keymap("n", "<C-d>", "<C-d>zz", opts)
vim.api.nvim_set_keymap("n", "<C-u>", "<C-u>zz", opts)
-- search term in middle
vim.api.nvim_set_keymap("n", "n", "nzzzv", opts)
vim.api.nvim_set_keymap("n", "N", "Nzzzv", opts)

-- https://github.com/folke/which-key.nvim
-- NOTE: starts with "<Plug>", whichkey set noremap=false automatically
local wk = require "which-key"
local telefuncs = saferequire "telescope.builtin"
if not telefuncs then error "failed to load telefuncs" end

-- dap
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
    ["1"] = { ":bf<cr>", "1 buf" },
    ["2"] = { ":bf<cr>:bn<cr>", "2 buf" },
    ["3"] = { ":bf<cr>:2bn<cr>", "3 buf" },
    ["4"] = { ":bf<cr>:3bn<cr>", "4 buf" },
    ["5"] = { ":bf<cr>:3bn<cr>", "5 buf" },
    ["6"] = { ":bf<cr>:3bn<cr>", "6 buf" },
    ["7"] = { ":bf<cr>:3bn<cr>", "7 buf" },
    h = { ":Telescope heading<cr>", "goto heading" },
  },
  ["<leader>"] = {
    ["<leader>"] = {
      function()
        telefuncs.fd {
          hidden = true,
          find_command = {
            "fd",
            "--type",
            "f",
            "--hidden",
            "--follow",
            "--exclude",
            ".git",
            "--exclude",
            "node_modules",
            "--exclude",
            ".venv",
          },
        }
        -- if vim.fn.system "git rev-parse --git-dir 2> /dev/null" == "" then
        --   -- not in git repo
        --   telefuncs.fd()
        -- else
        --   telefuncs.git_files()
        -- end
      end,
      "find files",
    },
    c = {
      name = "code",
      c = { ":CommentToggle<cr>", "comment" },
      f = { ":Format<cr>", "format" },
      r = { ":Reload<cr>", "reload" },
      s = { ":SymbolsOutline<cr>", "symbols" },
      t = { ":TodoTelescope<cr>", "todo" },
      u = { ":UndotreeToggle<cr>", "undo tree" },
    },
    d = {
      name = "dubug",
      j = { function() require("dap.ext.vscode").load_launchjs() end, "load launch.json" },
      c = { function() require("dap").continue() end, "continue" },
      n = { function() require("dap").step_over() end, "step over" },
      i = { function() require("dap").step_into() end, "step into" },
      o = { function() require("dap").step_out() end, "step out" },
      d = { function() require("dap").toggle_breakpoint() end, "toggle bp" },
      B = { function() require("dap").set_breakpoint() end, "set bp" },
      e = { function() require("dap").set_exception_breakpoints() end, "exception bk" },
      b = { function() require("dap").set_breakpoint(nil, nil, vim.fn.input "Log point msg: ") end, "break on" },
      r = { function() require("dap").repl.open() end, "repl open" },
      l = { function() require("dap").run_last() end, "run last" },
      h = { function() require("dap.ui.widgets").hover() end, "hover" },
      p = { function() require("dap.ui.widgets").preview() end, "preview" },
      f = {
        function()
          local widgets = require "dap.ui.widgets"
          widgets.centered_float(widgets.frames)
        end,
        "open frames",
      },
      s = {
        function()
          local widgets = require "dap.ui.widgets"
          widgets.centered_float(widgets.scopes)
        end,
        "open scopes",
      },
    },
    t = {
      name = "toogle",
      u = { ":UndotreeToggle<cr><C-w><C-w>", "undo tree" },
      d = { function() require("dapui").toggle() end, "dap ui" },
      c = { ":ColorizerToggle<cr>", "colorizer" },
      l = { ":Lazy<cr>", "Lazy.nvim" },
      n = { ":NullLsInfo<cr>", "null-ls" },
      m = { ":Mason<cr>", "Mason.nvim" },
      w = {
        function()
          if vim.opt.wrap:get() then
            vim.opt.wrap = false
          else
            vim.opt.wrap = true
          end
        end,
        "line wrap",
      },
    },
    f = {
      name = "file",
      a = { ":!git add %<cr>", "git add current file" },
      m = { ":make<cr>", "make" },
      e = { ":Ex<cr>", "file explore" },
      x = { "<cmd>!chmod +x %<cr>", "current file +x" },
      f = {
        function() telefuncs.fd { hidden = true, no_ignore = true } end,
        "find all files",
      },
      r = {
        function()
          local old_name = vim.fn.expand "%"
          local new_name = vim.fn.input { prompt = "New file name: ", default = vim.fn.expand "%", completion = "file" }
          if new_name ~= "" and new_name ~= old_name then
            vim.cmd("saveas " .. new_name)
            vim.cmd("silent !rm " .. old_name)
            vim.cmd("bd " .. old_name)
            vim.cmd "redraw!"
          end
        end,
        "rename file",
      },
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
      p = {
        function() require("telescope").extensions.project.project() end,
        "switch project",
      },
      s = { telefuncs.live_grep, "project search" },
    },
    h = {
      name = "help",
      i = { telefuncs.builtin, "telescope builtIn" },
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
      k = { ":bd!<cr>", "buffer delete" },
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
  -- navigate tab completion with <c-j> and <c-k>
  -- runs conditionally
  ["<C-j>"] = { 'pumvisible() ? "\\<C-n>" : "\\<C-j>"' },
  ["<C-k>"] = { 'pumvisible() ? "\\<C-p>" : "\\<C-k>"' },
}, { mode = "c", noremap = true })

wk.register({
  -- Better indenting
  ["<"] = "<gv",
  [">"] = ">gv",
  -- move selected line
  J = ":m '>+1<CR>gv=gv",
  K = ":m '<-2<CR>gv=gv",
  ["<leader>"] = {
    p = { [["_dP"]], "no yank paste" },
    d = { [["_d]], "no yank delete" },
  },
}, {
  mode = visual_mode,
  prefix = "",
  buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
  silent = true, -- use `silent` when creating keymaps
  noremap = true, -- use `noremap` when creating keymaps
  nowait = false, -- use `nowait` when creating keymaps
})

wk.register({
  ["<leader>"] = {
    p = { [["_dP"]], "no yank paste" },
    d = { [["_d]], "no yank delete" },
  },
}, { mode = visual_block_mode })
