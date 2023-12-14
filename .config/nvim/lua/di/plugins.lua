-- Install package manager if not installed
local lazypath = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system {
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  }
end
vim.opt.rtp:prepend(lazypath)

-- https://github.com/folke/lazy.nvim#-plugin-spec
require("lazy").setup({
  {
    "wuelnerdotexe/vim-astro",
    config = function() _G.astro_typescript = "enable" end,
  },
  -- |lsp related|
  "github/copilot.vim",
  {
    -- https://github.com/VonHeikemen/lsp-zero.nvim
    "VonHeikemen/lsp-zero.nvim",
    --import
    branch = "v1.x",
    dependencies = {
      -- LSP Support
      -- TODO delete branch property after relase > v0.1.7
      {
        "neovim/nvim-lspconfig", -- Required
        -- server name changed from "sumneko_lua" to "lua_ls" without change tag,
        -- hence need to pin to a specific branch
        branch = "master",
      },
      "williamboman/mason.nvim", -- Optional
      -- https://github.com/williamboman/mason-lspconfig.nvim
      "williamboman/mason-lspconfig.nvim", -- Optional

      -- Autocompletion
      "hrsh7th/nvim-cmp", -- Required
      "hrsh7th/cmp-nvim-lsp", -- Required
      "hrsh7th/cmp-buffer", -- Optional
      "hrsh7th/cmp-path", -- Optional
      "saadparwaiz1/cmp_luasnip", -- Optional
      "hrsh7th/cmp-nvim-lua", -- Optional

      -- Snippets
      "L3MON4D3/LuaSnip", -- Required
      "rafamadriz/friendly-snippets", -- Optional
    },
    config = function() require "di.lsp-zero" end,
  },
  {
    -- https://github.com/jose-elias-alvarez/null-ls.nvim
    "jose-elias-alvarez/null-ls.nvim",
    dependencies = {
      "williamboman/mason.nvim",
    },
    config = function() require "di.null-ls" end,
  },
  {
    "folke/neodev.nvim",
    config = function()
      require("neodev").setup {
        library = { plugins = { "nvim-dap-ui" }, types = true },
      }
    end,
  },
  -- |debugging|
  {
    -- https://github.com/mfussenegger/nvim-dap
    -- TODO config
    "mfussenegger/nvim-dap",
    dependencies = {
      "theHamsta/nvim-dap-virtual-text",
      "rcarriga/nvim-dap-ui",
      "mxsdev/nvim-dap-vscode-js",
    },
    keys = {
      { "<leader>dd", function() require("dap").toggle_breakpoint() end },
      { "<leader>dc", function() require("dap").continue() end },
    },
    config = function()
      local dap, dapui = require "dap", require "dapui"
      dapui.setup()
      dap.listeners.after.event_initialized["dapui_config"] = function() dapui.open() end
      dap.listeners.before.event_terminated["dapui_config"] = function() dapui.close() end
      dap.listeners.before.event_exited["dapui_config"] = function() dapui.close() end
      require("dap-vscode-js").setup {
        -- node_path = "node", -- Path of node executable. Defaults to $NODE_PATH, and then "node"
        -- debugger_path = "(runtimedir)/site/pack/packer/opt/vscode-js-debug", -- Path to vscode-js-debug installation.
        -- debugger_cmd = { "js-debug-adapter" }, -- Command to use to launch the debug server. Takes precedence over `node_path` and `debugger_path`.
        adapters = { "pwa-node", "pwa-chrome", "pwa-msedge", "node-terminal", "pwa-extensionHost" }, -- which adapters to register in nvim-dap
        -- log_file_path = "(stdpath cache)/dap_vscode_js.log" -- Path for file logging
        -- log_file_level = false -- Logging level for output to file. Set to false to disable file logging.
        -- log_console_level = vim.log.levels.ERROR -- Logging level for output to console. Set to false to disable console output.
      }
    end,
  },
  -- telescope
  {
    "nvim-telescope/telescope.nvim",
    config = function() require "di.telescope" end,
    dependencies = {
      "which-key.nvim",
      "nvim-lua/popup.nvim",
      "nvim-lua/plenary.nvim",
      {
        "crispgm/telescope-heading.nvim",
        config = function() require("telescope").load_extension "heading" end,
      },
      {
        -- https://github.com/nvim-telescope/telescope-project.nvim
        "nvim-telescope/telescope-project.nvim",
        config = function() require("telescope").load_extension "project" end,
      },
      -- {
      --   "nvim-telescope/telescope-frecency.nvim",
      --   dependencies = { "kkharji/sqlite.lua" },
      --   config = function() require("telescope").load_extension "frecency" end,
      -- },

      {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = "make",
        config = function() require("telescope").load_extension "fzf" end,
      },
      {
        "nvim-telescope/telescope-ui-select.nvim",
        config = function() require("telescope").load_extension "ui-select" end,
      },
      {
        "nvim-telescope/telescope-hop.nvim",
        config = function() require("telescope").load_extension "hop" end,
      },
    },
  },

  -- |treesitter|
  -- https://github.com/nvim-treesitter/nvim-treesitter#nvim-treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    dependencies = {
      "nvim-treesitter/playground",
      "nvim-treesitter/nvim-treesitter-refactor",
      -- https://github.com/nvim-treesitter/nvim-treesitter-textobjects
      "nvim-treesitter/nvim-treesitter-textobjects",
    },
    config = function() require "di.treesitter" end,
  },
  {
    -- https://github.com/echasnovski/mini.nvim/tree/stable
    "echasnovski/mini.nvim",
    version = nil, -- Main branch
    dependencies = { "kyazdani42/nvim-web-devicons" }, -- for tabline
    config = function()
      -- require("mini.ai").setup()
      require("mini.comment").setup() -- better comment
      require("mini.fuzzy").setup()
      require("mini.indentscope").setup()
      require("mini.jump").setup() -- multiple line f, F, t, T
      require("mini.jump2d").setup { mappings = { jump = "<CR>" } }
      require("mini.pairs").setup() -- auto insert paired text-object like
      require("mini.starter").setup()
      require("mini.surround").setup { mappings = { replace = "sc" } }
      require("mini.tabline").setup {
        set_vim_settings = false,
      }
      require("mini.trailspace").setup()
      require("mini.files").setup()
    end,
  },

  -- |colors|
  {
    -- https://github.com/norcalli/nvim-colorizer.lua
    "norcalli/nvim-colorizer.lua",
    cmd = { "ColorizerToggle", "ColorizerAttachToBuffer", "ColorizerReloadAllBuffers" },
    lazy = true,
    priority = 1000,
    config = function() require("colorizer").setup() end,
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
  -- |misc|
  "nvim-lua/plenary.nvim",

  {
    -- https://github.com/folke/which-key.nvim#%EF%B8%8F-configuration
    "folke/which-key.nvim",
    config = function()
      vim.o.timeout = true
      vim.o.timeoutlen = 300
      require("which-key").setup {
        key_labels = {
          ["<space>"] = "SPC",
          ["<cr>"] = "RET",
          ["<tab>"] = "TAB",
        },
      }
    end,
  },
  { "mbbill/undotree", cmd = { "UndotreeToggle", "UndotreeShow" } },
  -- { "vifm/vifm.vim" },
  {
    "stevearc/oil.nvim",
    opts = {},
    -- Optional dependencies
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function() require("oil").setup() end,
  },
  {
    "jackMort/ChatGPT.nvim",
    enabled = false,
    event = "VeryLazy",
    config = function()
      require("chatgpt").setup {
        --api_key_cmd = "pass openai/api_key"
      }
    end,
    dependencies = {
      "MunifTanjim/nui.nvim",
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
    },
  },
}, {
  defaults = {
    -- install stable version
    version = "*",
  },
})
