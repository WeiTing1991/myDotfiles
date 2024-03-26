-- vim.lsp.set_log_level("debug")

-- https://github.com/folke/neodev.nvim
require('neodev').setup({
  -- add any options here, or leave empty to use the default settings
})
-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
require('mason').setup({
  ui = {
    icons = {
      package_installed = '✓',
      package_pending = '➜',
      package_uninstalled = '✗',
    },
  },
})
-- LSP provides Neovim with features like:
--  - Go to definition
--  - Find references
--  - Autocompletion
--  - Symbol Search
--  - and more!
--
-- https://github.com/neovim/nvim-lspconfig/wiki/UI-customization

-- key mappings.
vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('lsp-attach', { clear = true }),
  callback = function(event)
    local map = function(keys, func, desc)
      vim.keymap.set('n', keys, func, { buffer = event.buf, desc = 'LSP: ' .. desc })
    end
    map('K', '<cmd>Lspsaga hover_doc<CR>', 'Hover Documentation')
    map('gd', require('telescope.builtin').lsp_definitions, '[G]oto [D]efinition')
    map('gr', require('telescope.builtin').lsp_references, '[G]oto [R]eferences')
    map('<C-j>', require('telescope.builtin').lsp_document_symbols, 'Document Symbols')
    map('<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', 'Buffer singture help')

    map('gI', require('telescope.builtin').lsp_implementations, '[G]oto [I]mplementation')
    map('<leader>D', require('telescope.builtin').lsp_type_definitions, 'Type [D]efinition')
    map('<leader>cr', vim.lsp.buf.rename, 'rename')
    map('<leader>ca', vim.lsp.buf.code_action, '[C]ode [A]ction')
    map('<leader>ck', '<cmd>lua vim.lsp.buf.type_definition()<CR>', 'type defintion')

    map('<leader>ws', require('telescope.builtin').lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')
    map('gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', '')
    map('<leader>cp', '<cmd>Lspsaga diagnostic_jump_next<CR>', '')
    map('<leader>cn', '<cmd>Lspsaga diagnostic_jump_prev<CR>', '')
    map('<leader>dl', '<cmd>Telescope diagnostics<cr>', 'Diagnostics')

    -- For example, in C this would take you to the header
    --map("gdc", vim.lsp.buf.declaration, "[G]oto [D]eclaration")

    local client = vim.lsp.get_client_by_id(event.data.client_id)
    if client and client.server_capabilities.documentHighlightProvider then
      vim.api.nvim_create_autocmd({ 'CursorHold', 'CursorHoldI' }, {
        buffer = event.buf,
        callback = vim.lsp.buf.document_highlight,
      })

      vim.api.nvim_create_autocmd({ 'CursorMoved', 'CursorMovedI' }, {
        buffer = event.buf,
        callback = vim.lsp.buf.clear_references,
      })
    end
  end,
})

-- See `:help CursorHold` for information about when this is executed
-- When you move your cursor, the highlights will be cleared (the second autocommand).

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = vim.tbl_deep_extend('force', capabilities, require('cmp_nvim_lsp').default_capabilities())

require('fidget').setup({ notification = { window = { winblend = 0 } } })

local ensure_installed = vim.tbl_keys(require('lsp.lsp-server') or {})
local formatting = vim.tbl_values(require('lsp.formatting'))
local dap_servers = vim.tbl_values(require('lsp.dap-server'))
--local linter = vim.tbl_values(require('lsp.linter'))

vim.list_extend(ensure_installed, formatting)
vim.list_extend(ensure_installed, dap_servers)
-- vim.list_extend(ensure_installed, linter)

require('mason-tool-installer').setup({
  automatic_installation = true,
  ensure_installed = ensure_installed,
})

require('mason-lspconfig').setup({
  handlers = {
    function(server_name)
      local server = require('lsp.lsp-server')[server_name] or {}

      -- This handles overriding only values explicitly passed
      -- by the server configuration above. Useful when disabling
      -- certain features of an LSP (for example, turning off formatting for tsserver)
      server.capabilities = vim.tbl_deep_extend('force', {}, capabilities, server.capabilities or {})
      require('lspconfig')[server_name].setup(server)
    end,
  },
})
require('mason-nvim-dap').setup({
  -- NOTE: it not work with mason-tool-installer
  automatic_installation = true,
  -- see mason-nvim-dap README for more information
  automatic_setup = true,
  ensure_installed = {},
})

vim.filetype.add({ extension = { templ = 'templ' } })
