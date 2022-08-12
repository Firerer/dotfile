return {
  -- https://code.mehalter.com/projects/68/files/master/which-key/register_mappings.lua
  n = {
    ["<leader>"] = {
      ["<leader>"] = { "<cmd>:Telescope find_files<cr>", "Find" },
      f = {
        s = { "<cmd>:w<cr>", "Save" },
      },
      b = {
        name = "Buffer",
        d = { "<cmd>:bd<CR>", "Kill buffer"},
      },
    },
  },
}
