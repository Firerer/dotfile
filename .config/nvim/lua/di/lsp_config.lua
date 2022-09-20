--[[
  default: https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md

]]
local wk = require "which-key"
local telefuncs = require "telescope.builtin"

local config = {}

local function caps()
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)
  return capabilities
end

config.capabilities = caps()

-- See `:help vim.diagnostic.*` for documentation on any of the below functions
-- See `:help vim.lsp.*` for documentation on any of the below functions
config.on_attach = function(client, bufnr)
    -- vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
    wk.register({
      K = { vim.lsp.buf.hover, "hover" },
      g = {
        name = "goto",
        d = { telefuncs.lsp_definitions, "definition" },
        D = { vim.lsp.buf.declaration, "declaration" },
        i = { vim.lsp.buf.implementation, "implementation" },
        r = { telefuncs.lsp_references, "references" },
      },
      ["["] = { d = { vim.diagnostic.goto_prev, "prev error" } },
      ["]"] = { d = { vim.diagnostic.goto_next, "next error" } },
      ["<leader>"] = {
        l = {
          name = "lsp",
          e = { vim.diagnostic.open_float, "show error" },
          q = { vim.diagnostic.setlocalist, "show all errors" },
          i = { ":LspInfo<cr>", "lsp info" },
          D = { vim.lsp.buf.type_definition, "type definition" },
          a = { vim.lsp.buf.code_action, "code action" },
          f = { vim.lsp.buf.formatting, "format" },
          r = { vim.lsp.buf.rename, "rename" },
          -- o = { require("jdtls").organize_imports, "java org imports" },
        },
        w = {
          name = "workspace",
          a = { vim.lsp.buf.add_workspace_folder, "add folder" },
          r = { vim.lsp.buf.remove_workspace_folder, "remove folder" },
          l = {
            function()
              print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
            end,
            "list folders",
          },
        },
      },
    }, { buffer = bufnr })
  end


config.flags = {
    -- This is the default in Nvim 0.7+
    debounce_text_changes = 150,
  }
  return config
