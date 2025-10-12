local keymaps = {}

local hover = vim.lsp.buf.hover
---@diagnostic disable-next-line: duplicate-set-field
vim.lsp.buf.hover = function()
  return hover({
    max_height = math.floor(vim.o.lines * 0.8),
    max_width = math.floor(vim.o.columns * 0.6),
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
      require("telescope.builtin").lsp_definitions()
    end, "Peek definition")
    map("gD", function()
      require("telescope.builtin").lsp_definitions()
    end, "Go to definition")
  end

  map("gi", function()
    require("telescope.builtin").lsp_implementations()
  end, "Goto Implementation")
  map("gr", function()
    require("telescope.builtin").lsp_references()
  end, "Find all References")

  map("gh", vim.lsp.buf.declaration, "Goto header declaration")

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

  map( "g.", function()
    local actions = {
      { name = "󰌵 Code Action", action = vim.lsp.buf.code_action },
      {
        name = "󰓆 Spell Suggest",
        action = function()
          require("telescope.builtin").spell_suggest()
        end,
      },
      {
        name = "Quick AI Action",
        action = function()
          require("sidekick.cli").prompt()
        end,
      },
    }
    vim.ui.select(actions, {
      prompt = "Quick Actions:",
      format_item = function(item)
        return item.name
      end,
    }, function(choice)
      if choice then
        choice.action()
      end
    end)
  end, "Code Action")

  map("<F2>", vim.lsp.buf.rename, "Rename in buf")
  map("gO", function()
    require("telescope.builtin").lsp_document_symbols()
  end, "Go to Symbol in File")

  map("gI", function()
    require("telescope.builtin").lsp_workspace_symbols()
  end, "Go to Symbol in Workspace")
  map("<S-i-o>", "<cmd>Neotree document_symbols toggle<cr>",  "document symbols" )

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
