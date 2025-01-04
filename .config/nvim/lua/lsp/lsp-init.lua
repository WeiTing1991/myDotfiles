require("mason").setup()
-- require("fidget").setup { notification = { window = { winblend = 0 } } }

local lsp_server = vim.tbl_keys(require "lsp.config.lsp-server" or {})
local lsp_lint_fomater = vim.tbl_values(require "lsp.config.lsp-extra")
-- local linter = vim.tbl_values(require "lsp.lsp-linter")
-- local dap_server = vim.tbl_values(require "lsp.dap-server")
local ensure_installed = {}

for _, value in ipairs(lsp_lint_fomater) do
  table.insert(ensure_installed, value)
end

-- for _, value in ipairs(dap_server) do
--   table.insert(ensure_installed, value)
-- end

require("mason-tool-installer").setup {
  ensure_installed = ensure_installed,
}

require("mason-lspconfig").setup {
  ensure_installed = lsp_server,
  automatic_installation = false,

  handlers = {
    function(server_name)
      local lspconfig = require "lspconfig"
      local server = lsp_server[server_name] or {}

      -- dissable javaserver attach here
      if server_name ~= "jdtls" then
        -- Useful when disabling
        -- certain features of an LSP (for example, turning off formatting for tsserver)
        server.capabilities = require("blink.cmp").get_lsp_capabilities(server.capabilities)

        -- disable pyright
        if server_name == "ruff_lsp" then
          server.server_capabilities.hoverProvider = false
        end

        lspconfig[server_name].setup(server)
      end
    end,
  },
}

-- key mappings.
vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("lsp-attach", { clear = true }),
  callback = function(args)
    local map = function(keys, func, desc, mode)
      mode = mode or "n"
      desc = desc or "No description"
      vim.keymap.set(mode, keys, func, { buffer = args.buf, desc = "LSP: " .. desc })
    end

    map("K", "<cmd>Lspsaga hover_doc<CR>", "Hover Documentation")
    map("gd", "<cmd>Lspsaga peek_definition<CR>", "Type [D]efinition")
    map("<leader>gd", require("telescope.builtin").lsp_definitions, "Goto Definition")
    map("gr", require("telescope.builtin").lsp_references, "Goto References")

    map("<C-j>", require("telescope.builtin").lsp_document_symbols, "Document Symbols")
    map("<C-k>", vim.lsp.buf.signature_help, "Buffer singture help")
    map("<C-o>", "<cmd>Lspsaga outline<CR>", "Buffer outline")

    map("gi", require("telescope.builtin").lsp_implementations, "Goto Implementation")
    -- map("gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", "")

    map("<leader>rn", vim.lsp.buf.rename, "rename")

    map("<leader>ca", vim.lsp.buf.code_action, "Code Action")
    map("<leader>D", require("telescope.builtin").lsp_type_definitions, "Type Definition")
    map("<leader>ck", vim.lsp.buf.type_definition, "Type defintion")
    map("<leader>,", vim.lsp.buf.format, "formatting")

    -- For example, in C this would take you to the header
    map("gD", vim.lsp.buf.declaration, "Goto Declaration")

    -- map("<leader>ws", require("telescope.builtin").lsp_dynamic_workspace_symbols, "[W]orkspace [S]ymbols")
    -- map("<leader>cp", "<cmd>Lspsaga diagnostic_jump_next<CR>", "")
    -- map("<leader>cn", "<cmd>Lspsaga diagnostic_jump_prev<CR>", "")
    -- map("<leader>dl", "<cmd>Telescope diagnostics<cr>", "Diagnostics")

    local client = vim.lsp.get_client_by_id(args.data.client_id)

    -- disable gopls semanticTokensProvider
    -- if client and client.name == "gopls" then
    --   if not client.server_capabilities.semanticTokensProvider then
    --     local semantic = client.config.capabilities.textDocument.semanticTokens
    --     client.server_capabilities.semanticTokensProvider = {
    --       full = true,
    --       legend = {
    --         tokenTypes = semantic.tokenTypes,
    --         tokenModifiers = semantic.tokenModifiers,
    --       },
    --       range = true,
    --     }
    --   end
    -- end

    -- Highlight the under cursor
    -- local client = vim.lsp.get_client_by_id(args.data.client_id)
    -- if client and client.supports_method(vim.lsp.protocol.Methods.textDocument_documentHighlight) then
    --   local highlight_augroup = vim.api.nvim_create_augroup('lsp-highlight', { clear = false })
    --   vim.api.nvim_create_autocmd({ 'CursorHold', 'CursorHoldI' }, {
    --     buffer = args.buf,
    --     group = highlight_augroup,
    --     callback = vim.lsp.buf.document_highlight,
    --   })
    --
    --   vim.api.nvim_create_autocmd({ 'CursorMoved', 'CursorMovedI' }, {
    --     buffer = args.buf,
    --     group = highlight_augroup,
    --     callback = vim.lsp.buf.clear_references,
    --   })
    --
    --   vim.api.nvim_create_autocmd('LspDetach', {
    --     group = vim.api.nvim_create_augroup('lsp-detach', { clear = true }),
    --     callback = function(args2)
    --       vim.lsp.buf.clear_references()
    --       vim.api.nvim_clear_autocmds { group = 'lsp-highlight', buffer = args2.buf }
    --     end,
    --   })
    -- end

    -- Toggle Inlay Hints
    -- The following code creates a keymap to toggle inlay hints in your
    -- code, if the language server you are using supports them
    -- This may be unwanted, since they displace some of your code
    -- if client and client.supports_method(vim.lsp.protocol.Methods.textDocument_inlayHint) then
    --   map('<leader>th', function()
    --     vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled { bufnr = event.buf })
    --   end, 'Toggle Inlay Hints')
    -- end
  end,
})

-- extra register
vim.diagnostic.config {
  virtual_text = false,
  update_in_insert = false,
  -- sings = true,
  --   float = {
  --   }
}

local signs = {
  -- Error = "",
  -- Warn = "",
  -- Info = "",
  -- Hint = "",
  Error = " ",
  Warn = " ",
  Info = " ",
  Hint = " ",
}

for type, icon in pairs(signs) do
  vim.fn.sign_define(
    "DiagnosticSign" .. type,
    { text = icon, texthl = "DiagnosticSign" .. type, numhl = "DiagnosticSign" .. type }
  )
end
