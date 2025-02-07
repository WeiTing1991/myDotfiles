require("mason").setup ({ ui = {
    icons = {
      package_installed = "✓",
      package_pending = "➜",
      package_uninstalled = "✗",
    },
  },
})

require("fidget").setup { notification = { window = { winblend = 0 } } }

local lsp_server = vim.tbl_keys(require "lsp.config.lsp-server" or {})
local lsp_lint_fomater = vim.tbl_values(require "lsp.config.lsp-extra" or {})
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
      -- desc = desc or "No description"
      vim.keymap.set(mode, keys, func, { buffer = args.buf, desc = "LSP: " .. desc })
    end

    map("K", vim.lsp.buf.hover, "Hover Documentation")
    map("gd", vim.lsp.buf.definition, "Type Definition")
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
    -- map("<S-l>o", "<cmd>Lspsaga outline<CR>", "Buffer outline")
    map("<S-l>rb", require("nvchad.lsp.renamer"), "Rename in buffer")
    map("<S-l>rr", "<cmd>LspRestart<CR>", "Lsp restart")

    map("<S-l>ca", vim.lsp.buf.code_action, "Code Action")
    map("<S-l>d", vim.lsp.buf.type_definition, "Type defintion")

    map("<leader>,", vim.lsp.buf.format, "formatting")

    -- map("<S-l>rp", "<cmd>Lspsaga lsp_rename ++project<CR>", "Rename in project")
    -- map("K", "<cmd>Lspsaga hover_doc<CR>", "Hover Documentation")
    -- map("gd", "<cmd>Lspsaga peek_definition<CR>", "Type Definition")
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
    --   vim.api.nvim_create_autocmd({ "LspDetach" }, {
    --     group = vim.api.nvim_create_augroup("lsp-detach", { clear = true }),
    --     callback = function(args2)
    --       vim.api.nvim_clear_autocmds { group = "diagnostic-hover", buffer = args2.buf }
    --     end,
    --   })
    -- end

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

-- Hover diagnostic
local highlight_augroup = vim.api.nvim_create_augroup("diagnostic-hover", { clear = false })
local ns = vim.api.nvim_create_namespace "CurlineDiag"
vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    vim.api.nvim_create_autocmd("CursorHold", {
      group = highlight_augroup,
      buffer = args.buf,
      callback = function()
        -- Ensure the buffer exists and has diagnostics
        if vim.api.nvim_buf_is_valid(args.buf) then
          -- Check if the buffer is of a specific type (e.g., 'oil') and skip it if necessary
          if vim.bo[args.buf].filetype == "oil" then
            return
          end
        end
        pcall(vim.api.nvim_buf_clear_namespace, args.buf, ns, 0, -1)
        local hi = { "Error", "Warn", "Info", "Hint" }
        local curline = vim.api.nvim_win_get_cursor(0)[1]
        local diagnostics = vim.diagnostic.get(args.buf, { lnum = curline - 1 })
        local virt_texts = { { (" "):rep(4) } }
        for _, diag in ipairs(diagnostics) do
          virt_texts[#virt_texts + 1] = { diag.message, " " .. hi[diag.severity] }
        end

        -- inline
        -- vim.api.nvim_buf_set_extmark(args.buf, ns, curline - 1, 0, {
        --   virt_text = virt_texts,
        --   hl_mode = 'combine'
        -- })

        -- float win
        vim.diagnostic.open_float(nil, {
          focus = false,
          -- scope = "line",
          scope = "cursor",
          border = "rounded",
          header = "",
          prefix = "󱓻 ",
          source = virt_texts,
        })
      end,
    })
  end,
})

-- diagnostic
vim.diagnostic.config {
  virtual_text = false,
  underline = true,
  update_in_insert = true,
  sings = true,
}

-- Change signs for LSP diagnostics
local signs = {
  Error = " ",
  Warn = " ",
  Info = " ",
  Hint = "󰠠 ",
}

for type, icon in pairs(signs) do
  vim.fn.sign_define(
    "DiagnosticSign" .. type,
    { text = icon, texthl = "DiagnosticSign" .. type, numhl = "DiagnosticSign" .. type }
  )
end
