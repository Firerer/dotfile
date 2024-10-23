local null_ls = require("null-ls")
local mason = require("mason")


-- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/formatting
local formatting = null_ls.builtins.formatting
-- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/diagnostics
local diagnostics = null_ls.builtins.diagnostics

null_ls.setup {
  debug = false,
  sources = {
    formatting.prettier.with {
      extra_args = { "--trailing-comma", "all" },
    },
    formatting.black.with { extra_args = { "--fast" } },
    formatting.stylua.with { extra_args = { "--collapse-simple-statement=Always" } },
    formatting.shellharden,

    -- diagnostics.eslint,
    -- diagnostics.shellcheck,
    -- diagnostics.flake8
  },
}
