local packer = require "packer"
packer.reset() -- to avoid use any plugin twice when reload

local use = packer.use
use "wbthomason/packer.nvim"
use "nvim-lua/plenary.nvim"
-- optimizer
use {
  "lewis6991/impatient.nvim",
  config = function()
    require("impatient").enable_profile()
  end,
}

-- which-key
use { "folke/which-key.nvim" }

-- telescope
use {
  {
    "nvim-telescope/telescope.nvim",
    after = {
      "which-key.nvim",
    },
    requires = {
      "nvim-lua/popup.nvim",
      "nvim-lua/plenary.nvim",
      "BurntSushi/ripgrep",
      "nvim-telescope/telescope-fzf-native.nvim",
    },
    config = "require('di.telescope')",
  },
  {
    "nvim-telescope/telescope-frecency.nvim",
    after = "telescope.nvim",
    requires = "kkharji/sqlite.lua",
    config = [[ require "telescope".load_extension("frecency") ]],
  },
  {
    "nvim-telescope/telescope-fzf-native.nvim",
    after = "telescope.nvim",
    run = "make",
    config = [[ require('telescope').load_extension('fzf') ]],
  },
  {
    "nvim-telescope/telescope-ui-select.nvim",
    after = "telescope.nvim",
    config = [[ require("telescope").load_extension("ui-select") ]],
  },
  "crispgm/telescope-heading.nvim",
  "nvim-telescope/telescope-file-browser.nvim",
  {
    "nvim-telescope/telescope-project.nvim",
    requires = "telescope.nvim",
    config = function()
      require("which-key").register {
        ["<leader>pp"] = {
          ":lua require'telescope'.extensions.project.project{}<CR>",
          "switch project",
        },
      }
    end,
  },
}

use {
  "jose-elias-alvarez/null-ls.nvim",
  requires = { "nvim-lua/plenary.nvim" },
  config = [[ require "di.null_ls" ]],
}

-- https://github.com/echasnovski/mini.nvim/tree/stable
use {
  "echasnovski/mini.nvim",
  branch = "stable",
  requires = { "kyazdani42/nvim-web-devicons" }, -- for tabline
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
    require("mini.sessions").setup()

    require("mini.trailspace").setup()
    require("mini.starter").setup()
    require("mini.indentscope").setup()
    require("mini.tabline").setup()

    require("mini.fuzzy").setup()
  end,
}

-- |lsp|
use {
  {
    "neovim/nvim-lspconfig",
    after = {
      "which-key.nvim", -- setting keybinds in config
      "telescope.nvim",
    },
    config = [[require('di.lsp')]],
  },
  "folke/trouble.nvim",
  "ray-x/lsp_signature.nvim",
  {
    "kosayoda/nvim-lightbulb",
    requires = "antoinemadec/FixCursorHold.nvim",
  },
}

-- |downloader|
use {
  {
    "williamboman/mason.nvim",
    config = function()
      require("mason").setup {
        ui = {
          icons = {
            package_installed = "✓",
            package_pending = "➜",
            package_uninstalled = "✗",
          },
        },
      }
    end,
  },
  {
    "williamboman/mason-lspconfig.nvim",
    requires = { "neovim/nvim-lspconfig", "williamboman/mason.nvim" },
    config = function()
      require("mason-lspconfig").setup {
        ensure_installed = {},
        automatic_installation = true, -- or { exclude: string[] } or false
      }
    end,
  },
}

use {
  "nvim-treesitter/nvim-treesitter",
  requires = {
    "nvim-treesitter/nvim-treesitter-refactor",
    "RRethy/nvim-treesitter-textsubjects",
  },
  config = "require('di.treesitter')",
  run = ":TSUpdate",
}
use {
  {
    "L3MON4D3/LuaSnip",
    event = "InsertEnter",
    config = function()
      require("luasnip.loaders.from_vscode").lazy_load()
    end,
  },
  "rafamadriz/friendly-snippets",
}

-- Completion
-- https://github.com/hrsh7th/nvim-cmp/wiki/List-of-sources
use {
  "hrsh7th/nvim-cmp",
  requires = {
    "hrsh7th/cmp-nvim-lsp",
    "lukas-reineke/cmp-under-comparator",
    "onsails/lspkind.nvim",
    { "hrsh7th/cmp-buffer", after = "nvim-cmp" },
    { "hrsh7th/cmp-nvim-lsp-document-symbol", after = "nvim-cmp" },
    { "hrsh7th/cmp-nvim-lsp-signature-help", after = "nvim-cmp" },
    { "hrsh7th/cmp-nvim-lua", after = "nvim-cmp" },
    { "hrsh7th/cmp-path", after = "nvim-cmp" },
    { "hrsh7th/cmp-cmdline", after = "nvim-cmp" },
    { "saadparwaiz1/cmp_luasnip", after = "nvim-cmp" },
  },
  config = [[ require "di.cmp" ]],
  event = "InsertEnter",
  after = "LuaSnip",
}

-- colors
-----
use {
  "norcalli/nvim-colorizer.lua",
  event = { "BufRead", "BufNewFile" },
  config = [[ require "colorizer".setup()]],
}

-- colorizer pairs
use {
  "frazrepo/vim-rainbow",
  event = { "BufRead", "BufNewFile" },
  config = function()
    vim.g.rainbow_active = 1
  end,
}

use {
  "RRethy/nvim-base16",
  disable = true,
  config = "vim.cmd [[colorscheme base16-google-dark]]",
}

use {
  "catppuccin/nvim",
  as = "catppuccin",
  config = [[require "di.catppuccin" ]],
}

if PACKER_BOOTSTRAP then
  require("packer").sync()
end
