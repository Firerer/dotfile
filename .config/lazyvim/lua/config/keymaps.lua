-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

local map = LazyVim.safe_keymap_set

-- Buffer
map("n", "<leader>bk", LazyVim.ui.bufremove, { desc = "Delete Buffer" })

-- Help-related keymaps under <leader>h
map("n", "<leader>hi", function()
  require("telescope.builtin").builtin()
end, { desc = "Telescope BuiltIn" })
map("n", "<leader>hh", function()
  require("telescope.builtin").help_tags()
end, { desc = "Help Tags" })
map("n", "<leader>hk", function()
  require("telescope.builtin").keymaps()
end, { desc = "Find Keymaps" })
map("n", "<leader>ht", function()
  require("telescope.builtin").colorscheme()
end, { desc = "Find Theme" })
map("n", "<leader>hc", function()
  require("telescope.builtin").commands()
end, { desc = "Commands" })
map("n", "<leader>hs", function()
  require("telescope.builtin").symbols()
end, { desc = "Symbols" })
map("n", "<leader>hm", function()
  require("telescope.builtin").man_pages()
end, { desc = "Man Pages" })
