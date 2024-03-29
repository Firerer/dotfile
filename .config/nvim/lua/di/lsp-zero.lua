-- https://github.com/VonHeikemen/lsp-zero.nvim
-- Learn the keybindings, see :help lsp-zero-keybindings
-- Learn to configure LSP servers, see :help lsp-zero-api-showcase
local lsp = require "lsp-zero"
local nvim_lsp = require "lspconfig"

lsp.preset "recommended"

lsp.ensure_installed {
  "tsserver",
  "eslint",
  "html",
  -- "denols",
  -- "astro",
  -- "svelte",
  -- "prismals",

  -- "bashls",
  -- "clangd",
  "pyright",
  "rust_analyzer",
  "lua_ls",

  "nil_ls",
  "jsonls",
  "marksman",
  -- "texlab"
  -- "docker_compose_language_service",
}

-- for options see doc lspconfig-all
lsp.configure("lua_ls", {
  settings = {
    Lua = {
      format = { enable = false },
      runtime = { version = "LuaJIT" },
      diagnostics = { globals = { "vim" } },
      workspace = {
        library = vim.api.nvim_get_runtime_file("", true),
        checkThirdParty = false, -- https://github.com/neovim/nvim-lspconfig/issues/1700#issuecomment-1033127328
      },
      telemetry = { enable = false },
    },
  },
})

lsp.configure("rust_analyzer", {
  settings = {
    ["rust-analyzer"] = {
      completion = {
        autoimport = {
          enable = true,
        },
      },
    },
  },
})

lsp.configure("pyright", {
  root_dir = nvim_lsp.util.root_pattern(
    "pyproject.toml",
    "setup.py",
    "setup.cfg",
    "requirements.txt",
    "Pipfile",
    ".git"
  ),
})

lsp.configure("tsserver", {
  single_file_support = false,
  root_dir = nvim_lsp.util.root_pattern "package.json",
})

lsp.configure("denols", {
  single_file_support = false,
  root_dir = nvim_lsp.util.root_pattern("deno.json", "import_map.json?"),
})

lsp.configure("prismals", {
  single_file_support = true,
})

local cmp = require "cmp"
local cmp_select = { behavior = cmp.SelectBehavior.Select }
local cmp_mappings = lsp.defaults.cmp_mappings {
  ["<C-k>"] = cmp.mapping.select_prev_item(cmp_select),
  ["<C-j>"] = cmp.mapping.select_next_item(cmp_select),
  ["<C-y>"] = cmp.mapping.confirm { select = true },
  -- disabled for copilot setup
  ["<Tab>"] = vim.NIL,
  ["<S-Tab>"] = vim.NIL,
}

lsp.setup_nvim_cmp {
  mapping = cmp_mappings,
}

lsp.on_attach(function(client, bufnr)
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

lsp.setup()

vim.diagnostic.config {
  virtual_text = true,
}
