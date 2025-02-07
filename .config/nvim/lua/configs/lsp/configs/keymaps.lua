-- key mappings.
vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("lsp-attach", { clear = true }),
  callback = function(args)
    local map = function(keys, func, desc, mode)
      mode = mode or "n"
      -- desc = desc or "No description"
      vim.keymap.set(mode, keys, func, { buffer = args.buf, desc = "LSP: " .. desc })
    end

    -- map("K", vim.lsp.buf.hover, "Hover Documentation")
    -- map("gd", vim.lsp.buf.definition, "Type Definition")
    map("K", "<cmd>Lspsaga hover_doc<CR>", "Hover Documentation")
    map("gd", "<cmd>Lspsaga peek_definition<CR>", "Type Definition")
    map("<leader>gd", require("telescope.builtin").lsp_definitions, "Goto Definition")
    -- For example, in C this would take you to the header
    map("gD", vim.lsp.buf.declaration, "Goto Declaratiokn")

    map("gr", vim.lsp.buf.references, "Goto References")
    map("gR", require("telescope.builtin").lsp_references, "Goto References")

    map("gi", vim.lsp.buf.implementation, "Peek Implementation")
    map("gI", require("telescope.builtin").lsp_implementations, "Goto Implementation")

    map("<S-l>j", require("telescope.builtin").lsp_document_symbols, "Document Symbols")
    map("<S-l>k", vim.lsp.buf.signature_help, "Buffer singture help")

    -- extra
    map("<S-l>o", "<cmd>Lspsaga outline<CR>", "Buffer outline")
    map("<S-l>rb", require("nvchad.lsp.renamer"), "Rename in buffer")
    map("<S-l>rp", vim.lsp.buf.rename, "Rename in project")
    map("<S-l>rr", "<cmd>LspRestart<CR>", "Lsp restart")

    map("<S-l>ca", vim.lsp.buf.code_action, "Code Action")
    map("<S-l>d", vim.lsp.buf.type_definition, "Type defintion")

    map("<leader>,", vim.lsp.buf.format, "formatting")


    -- map("<S-l>rp", "<cmd>Lspsaga lsp_rename ++project<CR>", "Rename in project")
    -- map("<S-l>ca", "<cmd>Lspsaga code_action<CR>", "Code Action")
    -- map("<C-l>ck", require("telescope.builtin").lsp_type_definitions, "Type Definition")

    -- map("<S-l>dn", "<cmd>Lspsaga diagnostic_jump_next<CR>", "Next diagnostic")
    -- map("<S-l>dp", "<cmd>Lspsaga diagnostic_jump_prev<CR>", "Prev diagnostic")
    -- map("<leader>dl", "<cmd>Telescope diagnostics<cr>", "Diagnostics")
    -- map("<leader>ws", require("telescope.builtin").lsp_dynamic_workspace_symbols, "[W]orkspace [S]ymbols")

    local client = vim.lsp.get_client_by_id(args.data.client_id)


    -- disable semanticTokensProvider
    if client and client.supports_method "textDocument/semanticTokens" then
      client.server_capabilities.semanticTokensProvider = nil
      -- client.capabilities = vim.lsp.protocol.make_client_capabilities()
      -- client.capabilities.textDocument.completion.completionItem = {
      --   documentationFormat = { "markdown", "plaintext" },
      --   snippetSupport = true,
      --   preselectSupport = true,
      --   insertReplaceSupport = true,
      --   labelDetailsSupport = true,
      --   deprecatedSupport = true,
      --   commitCharactersSupport = true,
      --   tagSupport = { valueSet = { 1 } },
      --   resolveSupport = {
      --     properties = {
      --       "documentation",
      --       "detail",
      --       "additionalTextEdits",
      --     },
      --   },
      -- }
    end

    -- Toggle Inlay Hints
    -- The following code creates a keymap to toggle inlay hints in your
    -- code, if the language server you are using supports them
    -- This may be unwanted, since they displace some of your code
    -- if client and client.supports_method(vim.lsp.protocol.Methods.textDocument_inlayHint) then
    --   map('', function()
    --     vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled { bufnr = event.buf })
    --   end, 'Toggle Inlay Hints')
    -- end
  end,
})
