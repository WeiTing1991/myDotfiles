local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = vim.tbl_deep_extend("force", capabilities, require("cmp_nvim_lsp").default_capabilities())

require("fidget").setup({
  notification = {
    window = {
      winblend = 0,
    },
  },
})
require("mason").setup()
local ensure_installed = vim.tbl_keys(require("lsp.lsp-server") or {})
local formating = vim.tbl_values(require("lsp.formatting"))
vim.list_extend(ensure_installed, formating)

require("mason-tool-installer").setup({
  automatic_installation = true,
  ensure_installed = ensure_installed,
})
require("mason-lspconfig").setup({
  automatic_installation = true,
  handlers = {
    function(server_name)
      local server = require("lsp.lsp-server")[server_name] or {}

      -- This handles overriding only values explicitly passed
      -- by the server configuration above. Useful when disabling
      -- certain features of an LSP (for example, turning off formatting for tsserver)
      server.capabilities = vim.tbl_deep_extend("force", {}, capabilities, server.capabilities or {})
      require("lspconfig")[server_name].setup(server)
    end,
  },
})
require("mason-nvim-dap").setup({
  automatic_installation = true,
  ensure_installed = {},
  -- automatic_setup = true,
  handlers = {
    function(config)
      -- all sources with no handler get passed here

      -- Keep original functionality
      require("mason-nvim-dap").default_setup(config)
    end,
    -- firefox = function(config)
    -- 	require("mason-nvim-dap").default_setup(config)
    -- end,
  },
})
