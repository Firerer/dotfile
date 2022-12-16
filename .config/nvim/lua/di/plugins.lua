-- Install packer if not installed
local install_path = vim.fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
local is_bootstrap = false

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  is_bootstrap = true
  vim.fn.execute("!git clone --depth=1 https://github.com/wbthomason/packer.nvim " .. install_path)
  vim.cmd([[packadd packer.nvim]])
end

require "packer".startup { function(use)
  use "wbthomason/packer.nvim"
  use "nvim-lua/plenary.nvim"
  -- optimizer
  use "lewis6991/impatient.nvim"

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

  -- use {
  --   "jose-elias-alvarez/null-ls.nvim",
  --   requires = { "nvim-lua/plenary.nvim" },
  --   config = [[ require "di.null_ls" ]],
  -- }

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
      --require("mini.sessions").setup()

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
    {
      "folke/trouble.nvim",
      requires = "kyazdani42/nvim-web-devicons",
      config = function()
        require("trouble").setup {
          -- https://github.com/folke/trouble.nvim
        }
      end
    },
    { "ray-x/lsp_signature.nvim",
      config = function()
        -- https://github.com/ray-x/lsp_signature.nvim
        require("lsp_signature").setup()
      end },
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
          automatic_installation = false, -- or { exclude: string[] } or false
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

  use {
    "RRethy/nvim-base16",
    disable = true,
    config = "vim.cmd [[colorscheme base16-google-dark]]",
  }

  use {
    "catppuccin/nvim",
    as = "catppuccin",
    disable = false,
    config = [[ require "di.catppuccin" ]],
  }
  if is_bootstrap then
    require("packer").sync()
  end
end,
  config = {
    auto_clean = true,
    display = {
      prompt_border = "rounded",
      open_fn = function()
        local result, win, buf = require('packer.util').float({ border = 'single' })
        vim.api.nvim_win_set_option(win, "winhighlight", "NormalFloat:Normal")
        return result, win, buf
      end
    },
    profile = {
      enable = true,
      threshold = 1,
    },
  }
}
