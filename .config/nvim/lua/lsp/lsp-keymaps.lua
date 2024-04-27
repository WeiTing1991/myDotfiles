-- LSP provides Neovim with features like:
--  - Go to definition
--  - Find references
--  - Autocompletion
--  - Symbol Search
--  - and more!
--
-- https://github.com/neovim/nvim-lspconfig/wiki/UI-customization

-- key mappings.
vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("lsp-attach", { clear = true }),
  callback = function(event)
    local map = function(keys, func, desc)
      vim.keymap.set("n", keys, func, { buffer = event.buf, desc = "LSP: " .. desc })
    end
    map("K", "<cmd>Lspsaga hover_doc", "Hover Documentation")
    map("gd", "<cmd>Lspsaga peek_definition<CR>", "Type [D]efinition")
    map("<leader>gd", require("telescope.builtin").lsp_definitions, "[G]oto [D]efinition")
    map("gr", require("telescope.builtin").lsp_references, "[G]oto [R]eferences")

    map("<C-j>", require("telescope.builtin").lsp_document_symbols, "Document Symbols")
    map("<C-k>", vim.lsp.buf.signature_help, "Buffer singture help")

    map("gI", require("telescope.builtin").lsp_implementations, "[G]oto [I]mplementation")
    map("<leader>D", require("telescope.builtin").lsp_type_definitions, "Type [D]efinition")

    map("<leader>cr", vim.lsp.buf.rename, "rename")
    map("<leader>ca", vim.lsp.buf.code_action, "[C]ode [A]ction")
    map("<leader>ck", vim.lsp.buf.type_definition, "type defintion")

    map("<leader>ws", require("telescope.builtin").lsp_dynamic_workspace_symbols, "[W]orkspace [S]ymbols")
    map("gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", "")
    map("<leader>cp", "<cmd>Lspsaga diagnostic_jump_next<CR>", "")
    map("<leader>cn", "<cmd>Lspsaga diagnostic_jump_prev<CR>", "")
    map("<leader>dl", "<cmd>Telescope diagnostics<cr>", "Diagnostics")

    map("<leader>m", vim.lsp.buf.format, "formatting")
    -- For example, in C this would take you to the header
    --map("gdc", vim.lsp.buf.declaration, "[G]oto [D]eclaration")


    local client = vim.lsp.get_client_by_id(event.data.client_id)
    if client.supports_method "textDocument/documentHighlight" then
      if client and client.server_capabilities.documentHighlightProvider then
        vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
          buffer = event.buf,
          callback = vim.lsp.buf.document_highlight,
        })

        vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
          buffer = event.buf,
          callback = vim.lsp.buf.clear_references,
        })
      end
    end
  end,
})
