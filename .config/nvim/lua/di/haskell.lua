local lspconfig = require "lspconfig"
-- require ghcup
lspconfig.hls.setup(vim.tbl_deep_extend("force", require "di.lsp_config", {
  setting = {},
}))
