-- key mappings.

vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("lsp-attach", { clear = true }),
  callback = function(event)
    local map = function(keys, func, desc, mode)
      mode = mode or "n"
      -- desc = desc or "No"
      vim.keymap.set(mode, keys, func, { buffer = event.buf, desc = desc })
    end

    -- map("K", function() return vim.lsp.buf.hover() end, "Hover Documentation")

    map("K", "<cmd>Lspsaga hover_doc<cr>", "Hover Documentation")
    map("gd", "<cmd>Lspsaga peek_definition<CR>", "peek Definition")

    -- map("gd", vim.lsp.buf.definition, "Goto Definition")
    -- map("gd", "<cmd>FzfLua lsp_finder<cr>", "Goto Definition")
    map("<leader>gd", "<cmd>FzfLua lsp_definitions jump_to_single_result=true ignore_current_line=true<cr>",
      "Goto Definition")

    -- For example, in C this would take you to the header
    -- map("gD", vim.lsp.buf.declaration, "Goto Declaration")
    map("gD", "<cmd>FzfLua lsp_declarations jump_to_single_result=true ignore_current_line=true<cr>",
      "Goto Declaration")

    -- map("gr", vim.lsp.buf.references, "Goto References")
    map("gr", "<cmd>FzfLua lsp_references jump_to_single_result=true ignore_current_line=true<cr>",
      "Goto References")

    map("gi", vim.lsp.buf.implementation, "Peek Implementation")
    map("gI", "<cmd>FzfLua lsp_implementations <cr>",
      "Goto Implementation")
    --
    map("<S-l>j", "<cmd>FzfLua lsp_document_symbols<cr>", "Document Symbols")
    map("<S-l>k", function() return vim.lsp.buf.signature_help() end, "Buffer singture help")
    map("z=", "<cmd>FzfLua spell_suggest<cr>", "Spell sugeestion")

    -- extra
    map("<S-l>o", "<cmd>Outline<cr>", "Outline")
    map("<S-l>ii", "<cmd>LspInfo<cr>", "Lsp Info")
    map("<S-l>ir", "<cmd>LspRestart<cr>", "Lsp restart")
    map("<S-l>rb", require("nvchad.lsp.renamer"), "Rename in buffer")
    map("<S-l>rp", vim.lsp.buf.rename, "Rename in project")

    map("<S-l>d", vim.lsp.buf.type_definition, "Type defintion")
    map("<S-l>d", "<cmd>FzfLua lsp_typedefs jump_to_single_result=true ignore_current_line=true<cr>", "Type defintion")

    map("<S-l>ca", "<cmd>FzfLua lsp_code_actions<cr>", "Code Action")
    -- map("<S-l>ca", vim.lsp.buf.code_action, "Code Action")

    map("<leader>,", vim.lsp.buf.format, "formatting")

    -- Optional
    -- map("<S-l>rp", "<cmd>Lspsaga lsp_rename ++project<CR>", "Rename in project")
    -- map("<S-l>ca", "<cmd>Lspsaga code_action<CR>", "Code Action")
    -- map("<C-l>ck", require("telescope.builtin").lsp_type_definitions, "Type Definition")

    -- map("<S-l>j", require("telescope.builtin").lsp_document_symbols, "Document Symbols")
    -- map("<S-l>dn", "<cmd>Lspsaga diagnostic_jump_next<CR>", "Next diagnostic")
    -- map("<S-l>dp", "<cmd>Lspsaga diagnostic_jump_prev<CR>", "Prev diagnostic")
    -- map("<leader>dl", "<cmd>Telescope diagnostics<cr>", "Diagnostics")
    -- map("<leader>ws", require("telescope.builtin").lsp_dynamic_workspace_symbols, "[W]orkspace [S]ymbols")


    -- disable semanticTokensProvider
    local client = vim.lsp.get_client_by_id(event.data.client_id)
    if client and client.supports_method "textDocument/semanticTokens" then
      client.server_capabilities.semanticTokensProvider = nil
      client.capabilities = vim.lsp.protocol.make_client_capabilities()
      client.capabilities.textDocument.completion.completionItem = {
        documentationFormat = { "markdown", "plaintext" },
        snippetSupport = true,
        preselectSupport = true,
        insertReplaceSupport = true,
        labelDetailsSupport = true,
        deprecatedSupport = true,
        commitCharactersSupport = true,
        tagSupport = { valueSet = { 1 } },
        resolveSupport = {
          properties = {
            "documentation",
            "detail",
            "additionalTextEdits",
          },
        },
      }
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
