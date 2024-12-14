require("mason").setup()

-- https://github.com/neovim/nvim-lspconfig/wiki/UI-customization
-- key mappings.
vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("lsp-attach", { clear = true }),
  callback = function(event)
    local map = function(keys, func, desc, mode)
      mode = mode or 'n'
      vim.keymap.set(mode, keys, func, { buffer = event.buf, desc = 'LSP: ' .. desc })
    end
    map("K", "<cmd>Lspsaga hover_doc<CR>", "Hover Documentation")
    map("gd", "<cmd>Lspsaga peek_definition<CR>", "Type [D]efinition")
    map("<leader>gd", require("telescope.builtin").lsp_definitions, "Goto Definition")
    map("gr", require("telescope.builtin").lsp_references, "Goto References")

    map("<C-j>", require("telescope.builtin").lsp_document_symbols, "Document Symbols")
    map("<C-k>", vim.lsp.buf.signature_help, "Buffer singture help")

    map("gI", require("telescope.builtin").lsp_implementations, "Goto Implementation")
    map("<leader>D", require("telescope.builtin").lsp_type_definitions, "Type Definition")

    map("<leader>rn", vim.lsp.buf.rename, "rename")

    map("<leader>ca", vim.lsp.buf.code_action, "Code Action")
    map("<leader>ck", vim.lsp.buf.type_definition, "type defintion")

    -- map("<leader>ws", require("telescope.builtin").lsp_dynamic_workspace_symbols, "[W]orkspace [S]ymbols")
    -- map("gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", "")
    map("<leader>cp", "<cmd>Lspsaga diagnostic_jump_next<CR>", "")
    map("<leader>cn", "<cmd>Lspsaga diagnostic_jump_prev<CR>", "")
    map("<leader>dl", "<cmd>Telescope diagnostics<cr>", "Diagnostics")

    map("<leader>m", vim.lsp.buf.format, "formatting")

    -- For example, in C this would take you to the header
    --map("gdc", vim.lsp.buf.declaration, "[G]oto [D]eclaration")

    local client = vim.lsp.get_client_by_id(event.data.client_id)
    if client and client.supports_method(vim.lsp.protocol.Methods.textDocument_documentHighlight) then
      local highlight_augroup = vim.api.nvim_create_augroup('lsp-highlight', { clear = false })
      vim.api.nvim_create_autocmd({ 'CursorHold', 'CursorHoldI' }, {
        buffer = event.buf,
        group = highlight_augroup,
        callback = vim.lsp.buf.document_highlight,
      })

      vim.api.nvim_create_autocmd({ 'CursorMoved', 'CursorMovedI' }, {
        buffer = event.buf,
        group = highlight_augroup,
        callback = vim.lsp.buf.clear_references,
      })

      vim.api.nvim_create_autocmd('LspDetach', {
        group = vim.api.nvim_create_augroup('lsp-detach', { clear = true }),
        callback = function(event2)
          vim.lsp.buf.clear_references()
          vim.api.nvim_clear_autocmds { group = 'lsp-highlight', buffer = event2.buf }
        end,
      })
    end
    -- The following code creates a keymap to toggle inlay hints in your
    -- code, if the language server you are using supports them

    -- This may be unwanted, since they displace some of your code
    if client and client.supports_method(vim.lsp.protocol.Methods.textDocument_inlayHint) then
      map('<leader>th', function()
        vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled { bufnr = event.buf })
      end, 'Toggle Inlay Hints')
    end
  end,
})

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
  ensure_installed = ensure_installed,
}

require("mason-lspconfig").setup {
  ensure_installed = lsp_server,
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

-- -- extra register
vim.diagnostic.config {
  virtual_text = {
    source = "if_many",
    prefix = "● ",
  },
  -- update_in_insert = true,
  underline = true,
  severity_sort = true,
  float = {
    style = "minimal",
    border = "rounded",
--     source = "always",
--     header = "",
--     prefix = "",
  },
}

