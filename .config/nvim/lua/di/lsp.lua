  local lspconfig = require "lspconfig"
  lspconfig.pyright.setup(require "di.lsp_config")

  lspconfig.tsserver.setup(require "di.lsp_config")

  lspconfig.rust_analyzer.setup(require "di.lsp_config")

  require "di.lua"
  require "di.csv"
  lspconfig.sumneko_lua.setup(vim.tbl_deep_extend("force", require "di.lsp_config", {
    settings = {
      Lua = {
        diagnostics = {
          globals = { "vim" },
        },
        workspace = {
          library = {
            [vim.fn.expand "$VIMRUNTIME/lua"] = true,
            [vim.fn.stdpath "config" .. "/lua"] = true,
          },
        },
      },
    },
  }))

