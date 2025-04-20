local keymaps = {}

local hover = vim.lsp.buf.hover
---@diagnostic disable-next-line: duplicate-set-field
vim.lsp.buf.hover = function()
    return hover {
        max_height = math.floor(vim.o.lines * 0.5),
        max_width = math.floor(vim.o.columns * 0.4),
    }
end

function keymaps.on_attach(client, bufnr)
  ---@param keys string
  ---@param func string|function
  ---@param desc string
  ---@param mode? string|string[]
  local map = function(keys, func, desc, mode)
    mode = mode or "n"
    vim.keymap.set(mode, keys, func, { buffer = bufnr, desc = desc })
  end
  local methods = vim.lsp.protocol.Methods


  map("K", function() return vim.lsp.buf.hover() end, "Hover Documentation")

  if client:supports_method(methods.textDocument_definition) then
      map('gd', function()
          require('fzf-lua').lsp_definitions { jump1 = false }
      end, 'Peek definition')
      map('gD', function()
          require('fzf-lua').lsp_definitions { jump1 = true }
      end, 'Go to definition')
  end

  -- map("gd", "<cmd>FzfLua lsp_finder<cr>", "Goto Definition")
  -- map("<leader>gd", "<cmd>FzfLua lsp_definitions jump_to_single_result=true ignore_current_line=true<cr>",
  --   "Goto Definition")


  -- For example, in C this would take you to the header
  -- map("gD", vim.lsp.buf.declaration, "Goto Declaration")
  -- map("gD", "<cmd>FzfLua lsp_declarations jump_to_single_result=true ignore_current_line=true<cr>",
  --   "Goto Declaration")

  -- map("gr", vim.lsp.buf.references, "Goto References")
  -- map("gr", "<cmd>FzfLua lsp_references jump_to_single_result=true ignore_current_line=true<cr>",
  --   "Goto References")

  -- map("gi", vim.lsp.buf.implementation, "Peek Implementation")
  -- map("gI", "<cmd>FzfLua lsp_implementations <cr>",
  --   "Goto Implementation")
  --
  -- map("<S-l>j", "<cmd>FzfLua lsp_document_symbols<cr>", "Document Symbols")
  -- map("<S-l>k", function() return vim.lsp.buf.signature_help() end, "Buffer singture help")
  -- map("z=", "<cmd>FzfLua spell_suggest<cr>", "Spell sugeestion")

  -- extra
  -- map("<S-l>o", "<cmd>Outline<cr>", "Outline")
  -- map("<S-l>ii", "<cmd>LspInfo<cr>", "Lsp Info")
  -- map("<S-l>ir", "<cmd>LspRestart<cr>", "Lsp restart")
  -- map("<S-l>rb", require("nvchad.lsp.renamer"), "Rename in buffer")
  -- map("<S-l>rp", vim.lsp.buf.rename, "Rename in project")
  --
  -- map("<S-l>d", vim.lsp.buf.type_definition, "Type defintion")
  -- map("<S-l>d", "<cmd>FzfLua lsp_typedefs jump_to_single_result=true ignore_current_line=true<cr>", "Type defintion")

  -- map("<S-l>ca", "<cmd>FzfLua lsp_code_actions<cr>", "Code Action")
  -- map("<S-l>ca", vim.lsp.buf.code_action, "Code Action")

  -- map("<leader>,", vim.lsp.buf.format, "formatting", {"n","v"})
  map("<leader>,", function()
    require("conform").format { async = true, lsp_format = "fallback" }
  end, "Formatting", { "n", "v" })

  -- Optional
  -- map("<S-l>rp", "<cmd>Lspsaga lsp_rename ++project<CR>", "Rename in project")
  -- map("<S-l>ca", "<cmd>Lspsaga code_action<CR>", "Code Action")
  -- map("<C-l>ck", require("telescope.builtin").lsp_type_definitions, "Type Definition")

  -- map("<S-l>j", require("telescope.builtin").lsp_document_symbols, "Document Symbols")
  -- map("<S-l>dn", "<cmd>Lspsaga diagnostic_jump_next<CR>", "Next diagnostic")
  -- map("<S-l>dp", "<cmd>Lspsaga diagnostic_jump_prev<CR>", "Prev diagnostic")
  -- map("<leader>dl", "<cmd>Telescope diagnostics<cr>", "Diagnostics")
  -- map("<leader>ws", require("telescope.builtin").lsp_dynamic_workspace_symbols, "[W]orkspace [S]ymbols")
end

return keymaps
