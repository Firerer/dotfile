-- TODO
-- https://github.com/nvim-treesitter/nvim-treesitter-textobjects
-- use "nvim-treesitter/nvim-treesitter-textobjects"
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
vim.opt.foldlevelstart = 99
vim.opt.foldnestmax = 3
vim.opt.foldminlines = 5

require("nvim-treesitter.configs").setup {
  ensure_installed = {
    "bash",
    "comment",
    "fish",
    "help",
    "java",
    "json",
    "lua",
    "python",
    "regex",
    "rust",
    "sql",
  },

  ignore_install = {}, -- or "all"

  highlight = {
    enable = true,
    disable = {},

    autopairs = {
      enable = true,
    },
    indent = { enable = true },
  },
}

-- treesitter_config() -- setup early
