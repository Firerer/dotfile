-- https://github.com/VonHeikemen/lsp-zero.nvim
-- Learn the keybindings, see :help lsp-zero-keybindings
-- Learn to configure LSP servers, see :help lsp-zero-api-showcase
--
local lsp_zero = require "lsp-zero"

lsp_zero.on_attach(function(client, bufnr)
  -- see :help lsp-zero-keybindings
  -- to learn the available actions
  lsp_zero.default_keymaps { buffer = bufnr }
  local opts = { buffer = bufnr, remap = false }

  vim.keymap.set("i", "<C-h>", function() vim.lsp.buf.signature_help() end, opts)

  -- lsp related
  require("which-key").register({
    ["["] = { e = { vim.diagnostic.goto_prev, "prev error" } },
    ["]"] = { e = { vim.diagnostic.goto_next, "next error" } },
    ["<leader>"] = {
      l = {
        name = "lsp",
        s = { vim.lsp.buf.workspace_symbol, "workspace_symbol" },
        e = { vim.diagnostic.open_float, "show error" },
        q = { vim.diagnostic.setlocalist, "show all errors" },
        i = { ":LspInfo<cr>", "lsp info" },
        D = { vim.lsp.buf.type_definition, "type definition" },
        a = { vim.lsp.buf.code_action, "code action" },
        f = { function() vim.lsp.buf.format { async = true } end, "format" },
        r = { vim.lsp.buf.rename, "rename" },
        R = { vim.lsp.codelens.refresh, "refresh lens" },
      },
      w = {
        name = "workspace",
        a = { vim.lsp.buf.add_workspace_folder, "add folder" },
        r = { vim.lsp.buf.remove_workspace_folder, "remove folder" },
        l = {
          function() print(vim.inspect(vim.lsp.buf.list_workspace_folders())) end,
          "list folders",
        },
      },
    },
    K = { vim.lsp.buf.hover, "hover" },
    g = {
      name = "goto",
      d = { require("telescope.builtin").lsp_definitions, "definition" },
      D = { vim.lsp.buf.declaration, "declaration" },
      i = { vim.lsp.buf.implementation, "implementation" },
      r = { require("telescope.builtin").lsp_references, "references" },
    },
  }, { buffer = bufnr })
end)

require("mason").setup {}
require("mason-lspconfig").setup {
  ensure_installed = {
    "tsserver",
    "eslint",
    "html",
    "lua_ls",
    "jsonls",
    "marksman",
  },
  handlers = {
    function(server_name) require("lspconfig")[server_name].setup {} end,
    tsserver = function() require("lspconfig").tsserver.setup {} end,
    eslint = function() require("lspconfig").eslint.setup {} end,
    html = function() require("lspconfig").html.setup {} end,
    jsonls = function() require("lspconfig").jsonls.setup {} end,
    lua_ls = function() require("lspconfig").lua_ls.setup {} end,
  },
}

-- for options see doc lspconfig-all

-- lsp_zero.configure("denols", {
--   single_file_support = false,
--   root_dir = nvim_lsp.util.root_pattern("deno.json", "import_map.json?"),
-- })

-- lsp_zero.configure("prismals", {
--   single_file_support = true,
-- })

lsp_zero.set_sign_icons {
  error = "✘",
  warn = "▲",
  hint = "⚑",
  info = "",
}

vim.diagnostic.config {
  virtual_text = false,
  severity_sort = true,
  float = {
    style = "minimal",
    border = "rounded",
    source = "always",
    header = "",
    prefix = "",
  },
}

-- vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
--   underline = true,
--   update_in_insert = false,
--   virtual_text = { spacing = 4, prefix = "~" },
--   severity_sort = true,
-- })

local cmp = require "cmp"
local cmp_action = lsp_zero.cmp_action()

-- this is the function that loads the extra snippets
-- from rafamadriz/friendly-snippets
require("luasnip.loaders.from_vscode").lazy_load()

vim.opt.completeopt = { "menu", "menuone", "noselect" }

cmp.setup {
  -- note: if going to use lsp-kind (another plugin)
  -- replace the line below with the function from lsp-kind
  formatting = lsp_zero.cmp_format { details = true },
  preselect = "item",
  completion = {
    completeopt = "menu,menuone,noinsert",
  },
  window = {
    completion = cmp.config.window.bordered(),
    documentation = cmp.config.window.bordered(),
  },
  sources = {
    { name = "path" },
    { name = "nvim_lsp" },
    { name = "nvim_lua" },
    { name = "luasnip", keyword_length = 2 },
    { name = "buffer", keyword_length = 3 },
  },
  mapping = cmp.mapping.preset.insert {
    -- confirm completion item
    ["<CR>"] = cmp.mapping.confirm {
      -- copilot documentation says this is important.
      -- I don't know why.
      behavior = cmp.ConfirmBehavior.Replace,
      select = false,
    },

    -- toggle completion menu
    ["<C-e>"] = cmp_action.toggle_completion(),

    -- tab complete
    -- disabled for copilot setup
    ["<Tab>"] = vim.NIL,
    ["<S-Tab>"] = vim.NIL,

    -- navigate between snippet placeholder
    ["<C-d>"] = cmp_action.luasnip_jump_forward(),
    ["<C-b>"] = cmp_action.luasnip_jump_backward(),

    -- scroll documentation window
    ["<C-f>"] = cmp.mapping.scroll_docs(5),
    ["<C-u>"] = cmp.mapping.scroll_docs(-5),
  },
  snippet = {
    expand = function(args) require("luasnip").lsp_expand(args.body) end,
  },
}
