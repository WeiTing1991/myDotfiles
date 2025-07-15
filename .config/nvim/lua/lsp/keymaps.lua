local keymaps = {}

local hover = vim.lsp.buf.hover
---@diagnostic disable-next-line: duplicate-set-field
vim.lsp.buf.hover = function()
  return hover({
    max_height = math.floor(vim.o.lines * 0.5),
    max_width = math.floor(vim.o.columns * 0.4),
  })
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

  map("K", function()
    return vim.lsp.buf.hover()
  end, "Hover Documentation")

  if client:supports_method(methods.textDocument_definition) then
    map("gd", function()
      -- require("goto-preview").goto_preview_definition()
      require("telescope.builtin").lsp_definitions()
    end, "Peek definition")
    map("gD", function()
      require("telescope.builtin").lsp_definitions()
    end, "Go to definition")
  end

  -- map("gr", vim.lsp.buf.references, "Goto References")
  -- map("gr", function()
  --   require("goto-preview").goto_preview_references()
  -- end, "Preview References")

  -- map("grr", "<cmd>FzfLua lsp_references<cr>", "Goto References")

  -- For example, in C this would take you to the header
  -- map("gl", vim.lsp.buf.declaration, "Goto C Header Declaration")
  -- map("gl", "<cmd>FzfLua lsp_finder<cr>", "Goto header Declaration")

  -- map("gi", "<cmd>lua require('goto-preview').goto_preview_implementation()<CR>", "Peek Implementation")
  -- map("gI", "<cmd>FzfLua lsp_implementations <cr>", "Goto Implementation")
  -- map("<S-l>j", "<cmd>FzfLua lsp_document_symbols<cr>", "Document Symbols")
  if client:supports_method(methods.textDocument_signatureHelp) then
    local blink_window = require("blink.cmp.completion.windows.menu")
    local blink = require("blink.cmp")
    map("<S-l>k", function()
      -- Close the completion menu first (if open).
      if blink_window.win:is_open() then
        blink.hide()
      end
      vim.lsp.buf.signature_help()
    end, "Signature help", "i")
  end
  -- extra
  map("g.", vim.lsp.buf.code_action, "Code Action")
  map("<S-l>ii", "<cmd>LspInfo<cr>", "Lsp Info")
  map("<S-l>ir", "<cmd>LspRestart<cr>", "Lsp restart")
  map("<S-l>r", vim.lsp.buf.rename, "Rename in buf")
  -- map("<S-l>d", "<cmd>FzfLua lsp_typedefs jump_to_single_result=true ignore_current_line=true<cr>", "Type defintion")
  -- map("<S-l>d", vim.lsp.buf.type_definition, "Type defintion")
  -- map("<S-l>ca", vim.lsp.buf.code_action, "Code Action")

  -- formatting
  map("<leader>,", vim.lsp.buf.format, "formatting", { "n", "v" })

  -- Add "Fix all" command for ESLint.
  -- if client.name == 'eslint' then
  --     vim.keymap.set('n', '<S-l>tl', function()
  --         if not client then
  --             return
  --         end
  --
  --         client:request(vim.lsp.protocol.Methods.workspace_executeCommand, {
  --             command = 'eslint.applyAllFixes',
  --             arguments = {
  --                 {
  --                     uri = vim.uri_from_bufnr(bufnr),
  --                     version = vim.lsp.util.buf_versions[bufnr],
  --                 },
  --             },
  --         }, nil, bufnr)
  --     end, { desc = 'Fix all ESLint errors', buffer = bufnr })
  -- end
end

return keymaps
