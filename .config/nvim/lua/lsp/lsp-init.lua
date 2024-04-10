-- https://github.com/folke/neodev.nvim
require("neodev").setup {
  -- add any options here, or leave empty to use the default settings
}
require("mason").setup {
  ui = {
    icons = {
      package_installed = "✓",
      package_pending = "➜",
      package_uninstalled = "✗",
    },
  },
}
-- LSP provides Neovim with features like:
--  - Go to definition
--  - Find references
--  - Autocompletion
--  - Symbol Search
--  - and more!
-- https://github.com/neovim/nvim-lspconfig/wiki/UI-customization

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = vim.tbl_deep_extend("force", capabilities, require("cmp_nvim_lsp").default_capabilities())

require("fidget").setup { notification = { window = { winblend = 0 } } }

local lsp_server = vim.tbl_keys(require "lsp.lsp-server" or {})

local formatter = vim.tbl_values(require "lsp.lsp-formater")
local linter = vim.tbl_values(require "lsp.lsp-linter")
local dap_server = vim.tbl_values(require "lsp.dap-server")

local ensure_installed = {}

for _, value in ipairs(formatter) do
  table.insert(ensure_installed, value)
end

for _, value in ipairs(linter) do
  table.insert(ensure_installed, value)
end

for _, value in ipairs(dap_server) do
  table.insert(ensure_installed, value)
end

require("mason-tool-installer").setup {
  automatic_installation = true,
  ensure_installed = ensure_installed,
}

require("mason-lspconfig").setup {
  ensure_installed = lsp_server,
  automatic_installation = true,
  handlers = {
    function(server_name)
      local server = require("lsp.lsp-server")[server_name] or {}
      -- dissable javaserver attach here
      if server_name ~= "jdtls" then
        -- Useful when disabling
        -- certain features of an LSP (for example, turning off formatting for tsserver)
        server.capabilities = vim.tbl_deep_extend("force", {}, capabilities, server.capabilities or {})
        require("lspconfig")[server_name].setup(server)

        -- if server_name == 'gopls' and not server.server_capabilities.semanticTokensProvider then
        --   local semantic = server.config.capabilities.textDocument.semanticTokens
        --   server.server_capabilities.semanticTokensProvider = {
        --     full = true,
        --     legend = {tokenModifiers = semantic.tokenModifiers, tokenTypes = semantic.tokenTypes},
        --     range = true,
        --   }
        -- end
      end

    end,
  },
}

--require('mason-nvim-dap').setup({
--  automatic_installation = true,
--  -- see mason-nvim-dap README for more information
--  automatic_setup = true,
--  ensure_installed = {},
--})

-- extra register
vim.filetype.add { extension = { templ = "templ" } }

vim.diagnostic.config {
}
