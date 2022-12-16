--[[
  default: https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
]]
local wk = require "which-key"
local telefuncs = require "telescope.builtin"

local config = {}

local function caps()
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)
  return capabilities
end

config.capabilities = caps()

-- See `:help vim.diagnostic.*` for documentation on any of the below functions
-- See `:help vim.lsp.*` for documentation on any of the below functions
config.on_attach = function(client, bufnr)
  -- vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
end


config.flags = {
  -- This is the default in Nvim 0.7+
  debounce_text_changes = 150,
}
return config
