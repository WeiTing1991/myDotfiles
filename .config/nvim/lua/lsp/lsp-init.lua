local cmp_nvim_lsp = require("cmp_nvim_lsp")
-- local navic = require("nvim-navic")

-- LSP stands for Language Server Protocol. It's a protocol that helps editors
-- and language tooling communicate in a standardized fashion.

-- LSP provides Neovim with features like:
--  - Go to definition
--  - Find references
--  - Autocompletion
--  - Symbol Search
--  - and more!
vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("lsp-attach", { clear = true }),
  callback = function(event)
    -- In this case, we create a function that lets us more easily define mappings specific
    -- for LSP related items. It sets the mode, buffer and description for us each time.
    local map = function(keys, func, desc)
      vim.keymap.set("n", keys, func, { buffer = event.buf, desc = "LSP: " .. desc })
    end

    --  To jump back, press <C-T>.
    map("gd", require("telescope.builtin").lsp_definitions, "[G]oto [D]efinition")

    -- Find references for the word under your cursor.
    map("gr", require("telescope.builtin").lsp_references, "[G]oto [R]eferences")

    -- Jump to the implementation of the word under your cursor.
    --  Useful when your language has ways of declaring types without an actual implementation.
    map("gI", require("telescope.builtin").lsp_implementations, "[G]oto [I]mplementation")

    -- Jump to the type of the word under your cursor.
    --  Useful when you're not sure what type a variable is and you want to see
    map("<leader>D", require("telescope.builtin").lsp_type_definitions, "Type [D]efinition")

    -- Fuzzy find all the symbols in your current document.
    --  Symbols are things like variables, functions, types, etc.
    map("<leader>ds", require("telescope.builtin").lsp_document_symbols, "[D]ocument [S]ymbols")

    -- Fuzzy find all the symbols in your current workspace
    --  Similar to document symbols, except searches over your whole project.
    map("<leader>ws", require("telescope.builtin").lsp_dynamic_workspace_symbols, "[W]orkspace [S]ymbols")

    -- Rename the variable under your cursor
    --  Most Language Servers support renaming across files, etc.
    map("<leader>rn", vim.lsp.buf.rename, "[R]e[n]ame")

    -- Execute a code action, usually your cursor needs to be on top of an error
    -- or a suggestion from your LSP for this to activate.
    map("<leader>ca", vim.lsp.buf.code_action, "[C]ode [A]ction")

    map("<leader>dl", "<cmd>Telescope diagnostics<cr>", "diagnostics")

    -- Opens a popup that displays documentation about the word under your cursor
    --  See `:help K` for why this keymap
    --map("K", vim.lsp.buf.hover, "Hover Documentation")
    map("K", "<cmd>Lspsaga hover_doc<CR>", "Hover Documentation")
    -- https://nvimdev.github.io/lspsaga/
    -- WARN: This is not Goto Definition, this is Goto Declaration.
    --  For example, in C this would take you to the header
    map("gdc", vim.lsp.buf.declaration, "[G]oto [D]eclaration")

    --    See `:help CursorHold` for information about when this is executed
    -- When you move your cursor, the highlights will be cleared (the second autocommand).
    local client = vim.lsp.get_client_by_id(event.data.client_id)
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
  end,
})
