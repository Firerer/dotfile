-- Install package manager if not installed
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  "nvim-lua/plenary.nvim",
  -- optimizer
  "lewis6991/impatient.nvim",
  -- https://github.com/ThePrimeagen/harpoon
  "theprimeagen/harpoon",

  -- which-key
  -- https://github.com/folke/which-key.nvim#%EF%B8%8F-configuration
  {
    "folke/which-key.nvim",
    config = function()
      vim.o.timeout = true
      vim.o.timeoutlen = 300
      require("which-key").setup({
        ["<space>"] = "SPC",
        ["<cr>"] = "RET",
        ["<tab>"] = "TAB",
      })
    end,
  },
  { "mbbill/undotree", cmd = { "UndotreeToggle", "UndotreeShow" } },

  { "tpope/vim-fugitive", },

  -- telescope
  {
    "nvim-telescope/telescope.nvim",
    config = function() require('di.telescope') end,
    dependencies = {
      "which-key.nvim",
      "nvim-lua/popup.nvim",
      "nvim-lua/plenary.nvim",
      {
        "crispgm/telescope-heading.nvim",
        config = function() require('telescope').load_extension('heading') end,
      },
      {
        -- https://github.com/nvim-telescope/telescope-project.nvim
        "nvim-telescope/telescope-project.nvim",
        config = function() require('telescope').load_extension('project') end,
      },
      {
        "nvim-telescope/telescope-frecency.nvim",
        dependencies = { "kkharji/sqlite.lua" },
        config = function() require "telescope".load_extension("frecency") end,
      },

      {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = "make",
        config = function() require('telescope').load_extension('fzf') end,
      },
      {
        "nvim-telescope/telescope-ui-select.nvim",
        config = function() require("telescope").load_extension("ui-select") end,
      },
      {
        "nvim-telescope/telescope-hop.nvim",
        config = function() require("telescope").load_extension('hop') end,
      },
    },
  },

  -- |treesitter|
  {
    "nvim-treesitter/nvim-treesitter",
    config = function() require('di.treesitter') end,
    build = ":TSUpdate",
    dependencies = {
      "nvim-treesitter/playground",
      "nvim-treesitter/nvim-treesitter-refactor",
      "RRethy/nvim-treesitter-textsubjects",
    },
  },

  -- https://github.com/echasnovski/mini.nvim/tree/stable
  {
    "echasnovski/mini.nvim",
    branch = "stable",
    dependencies = { "kyazdani42/nvim-web-devicons" }, -- for tabline
    config = function()
      -- better comment
      require("mini.comment").setup()
      require("mini.ai").setup()
      -- auto insert paired text-object like
      require("mini.pairs").setup()
      -- multiple line f, F, t, T
      require("mini.jump").setup()
      -- jump to any location in window
      require("mini.jump2d").setup()
      -- surround
      require("mini.surround").setup { mappings = {
        replace = "sc",
      } }
      --require("mini.sessions").setup()

      require("mini.trailspace").setup()
      require("mini.starter").setup()
      require("mini.indentscope").setup()
      require("mini.tabline").setup()

      require("mini.fuzzy").setup()
    end,
  },
  {
    -- https://github.com/VonHeikemen/lsp-zero.nvim
    'VonHeikemen/lsp-zero.nvim',
    branch = 'v1.x',
    dependencies = {
      -- LSP Support
      { 'neovim/nvim-lspconfig' }, -- Required
      { 'williamboman/mason.nvim' }, -- Optional
      { 'williamboman/mason-lspconfig.nvim' }, -- Optional

      -- Autocompletion
      { 'hrsh7th/nvim-cmp' }, -- Required
      { 'hrsh7th/cmp-nvim-lsp' }, -- Required
      { 'hrsh7th/cmp-buffer' }, -- Optional
      { 'hrsh7th/cmp-path' }, -- Optional
      { 'saadparwaiz1/cmp_luasnip' }, -- Optional
      { 'hrsh7th/cmp-nvim-lua' }, -- Optional

      -- Snippets
      { 'L3MON4D3/LuaSnip' }, -- Required
      { 'rafamadriz/friendly-snippets' }, -- Optional
    },
  },


  -- colors
  -----
  {
    -- https://github.com/norcalli/nvim-colorizer.lua
    "norcalli/nvim-colorizer.lua",
    cmd = { "ColorizerToggle", "ColorizerAttachToBuffer", "ColorizerReloadAllBuffers" },
    lazy = true,
    config = function() require "colorizer".setup() end,
  },
  {
    "RRethy/nvim-base16",
    enabled = false,
    config = function() vim.cmd [[colorscheme base16-google-dark]] end,
  },
  {
    "catppuccin/nvim",
    name = "catppuccin",
    config = function() require "di.catppuccin" end,
  },
}, {
  defaults = {
    -- install stable version
    version = "*"
  }
})
