local keymaps = {}

local hover = vim.lsp.buf.hover
---@diagnostic disable-next-line: duplicate-set-field
vim.lsp.buf.hover = function()
  return hover({
    max_height = math.floor(vim.o.lines * 0.8),
    max_width = math.floor(vim.o.columns * 0.6),
  })
end

local function client_supports_method(client, method, bufnr)
  if vim.fn.has("nvim-0.11") == 1 then
    return client:supports_method(method, bufnr)
  else
    return client.supports_method(method, { bufnr = bufnr })
  end
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
      vim.lsp.buf.definition()
    end, "Go to definition ", { "n", "v" })

    map("gD", function()
      vim.cmd("vsplit")
      vim.lsp.buf.definition()
    end, "Go to definition with split", { "n", "v" })
  end

  map("gi", function()
    vim.lsp.buf.implementation()
  end, "Goto Implementation")
  map("gr", vim.lsp.buf.references, "Find all References")
  map("gR", function()
    require("fzf-lua").lsp_references()
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

  map("g.", function()
    local actions = {
      { name = "Code action", action = vim.lsp.buf.code_action },
      {
        name = "Spell suggest",
        action = function()
          require("fzf-lua").spell_suggest()
        end,
      },
      {
        name = "Quick ai action",
        action = function()
          require("sidekick.cli").prompt()
        end,
      },
    }

    local names = {}
    for _, item in ipairs(actions) do
      table.insert(names, item.name)
    end

    vim.ui.select(names, {
      prompt = "Quick Actions> ",
    }, function(choice)
      if choice then
        for _, item in ipairs(actions) do
          if item.name == choice then
            item.action()
            break
          end
        end
      end
    end)
  end, "Code Action")

  map("<F2>", require("nvchad.lsp.renamer"), "Rename in buf")
  map("gO", function()
    require("fzf-lua").lsp_document_symbols()
  end, "Go to Symbol in File")
  map("go", "<cmd>topleft Outline<CR>", "Document Symbols")

  map("gW", function()
    require("fzf-lua").lsp_workspace_symbols()
  end, "Go to Symbol in Workspace")
  -- map("<S-i-o>", "<cmd>Neotree document_symbols toggle<cr>", "document symbols")

  map("<leader>,", vim.lsp.buf.format, "formatting", { "n", "v" })

  -- Inlay hints toggle
  if client and client_supports_method(client, vim.lsp.protocol.Methods.textDocument_inlayHint, bufnr) then
    map('<leader>th', function()
      vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled { bufnr = bufnr })
    end, 'Toggle Inlay Hints')
  end

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
