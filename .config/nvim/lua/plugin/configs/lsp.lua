local icons = require "icon"

require("mason").setup {
  ui = {
    icons = icons.install,
  },
}
require("fidget").setup { notification = { window = { winblend = 0 } } }

local ensure_installed = {}
local lsp_server = vim.tbl_keys(require "lsp.server" or {})
local lsp_extra = vim.tbl_values(require "lsp.formater_linter" or {})
local debugger_server = require("lsp.debugger") or {}


vim.list_extend(ensure_installed, lsp_server)
vim.list_extend(ensure_installed, debugger_server)
if not lsp_extra[1] then
  vim.list_extend(ensure_installed, lsp_extra)
end

require("mason-tool-installer").setup {
  ensure_installed = ensure_installed,
  run_on_start = true,
}

-- Setup servers
require("mason-lspconfig").setup {
  ensure_installed = {},
  automatic_installation = false,
  handlers = {
    function(server_name)
      local lspconfig = require "lspconfig"
      local ls_server_config = require "lsp.server" or {}
      local server = ls_server_config[server_name] or {}

      -- Useful when disabling
      -- dissable typscript/javaserver attach here
      -- if server_name == "jdtls" or server_name == "ts_ls" then
      --   return
      -- end

      -- if server_name == "ruff_lsp" then
      --   -- if server.server_capabilities == nil then
      --   --   server.server_capabilities = {}
      --   -- end
      --   server.server_capabilities.hoverProvider = false
      --   server.server_capabilities.documentHighlightProvider = false
      -- end

      -- Start by making a basic capabilities object
      local capabilities = vim.lsp.protocol.make_client_capabilities()

      -- Extend it with cmp or blink capabilities
      capabilities = require("blink.cmp").get_lsp_capabilities(capabilities)

      -- Add folding capabilities
      capabilities.textDocument.foldingRange = {
        dynamicRegistration = false,
        lineFoldingOnly = true,
      }

      -- Use the extended capabilities
      server.capabilities = vim.tbl_deep_extend("force", capabilities, server.capabilities or {})
      lspconfig[server_name].setup(server)
    end,
  },
}

-- Setup Keymaps
vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("wtc/lsp_attach", { clear = true }),
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    if not client then
      return
    end

    local lsp_keymap = require "lsp.Keymaps"
    lsp_keymap.on_attach(client, args.buf)
  end,
})

-- Diagnostic configuration.
local diagnostic_icons = icons.diagnostics

vim.diagnostic.config{
  signs = {
    text = {
      [vim.diagnostic.severity.ERROR] = diagnostic_icons.Error,
      [vim.diagnostic.severity.WARN] = diagnostic_icons.Warn,
      [vim.diagnostic.severity.INFO] = diagnostic_icons.Info,
      [vim.diagnostic.severity.HINT] = diagnostic_icons.Hint,
    },
    texthl = {
      [vim.diagnostic.severity.ERROR] = "DiagnosticSignError",
      [vim.diagnostic.severity.WARN] = "DiagnosticSignWarn",
      [vim.diagnostic.severity.INFO] = "DiagnosticSignInfo",
      [vim.diagnostic.severity.HINT] = "DiagnosticSignHint",
    },
    numhl = {
      [vim.diagnostic.severity.ERROR] = "DiagnosticSignError",
      [vim.diagnostic.severity.WARN] = "DiagnosticSignWarn",
      [vim.diagnostic.severity.INFO] = "DiagnosticSignInfo",
      [vim.diagnostic.severity.HINT] = "DiagnosticSignHint",
    }
  },
}

-- Hover diagnostic
vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("wtc/diagnostic-hover", { clear = false }),
  callback = function(args)
    local g = g or {}
    g.diagnostic_float_winid = nil
    g.diagnostic_float_line = nil

    -- Ensure the buffer exists and has diagnostics
    if vim.api.nvim_buf_is_valid(args.buf) then
      if vim.bo[args.buf].filetype == "oil" then
        return
      end

      -- Set up a keymap to show diagnostics float
      vim.keymap.set("n", "<C-m>", function()
        local curline = vim.api.nvim_win_get_cursor(0)[1]
        local diagnostics = vim.diagnostic.get(args.buf, { lnum = curline - 1 })
        local virt_texts = { { (" "):rep(4) } }
        local hi = { "Error", "Warn", "Info", "Hint" }

        for _, diag in ipairs(diagnostics) do
          virt_texts[#virt_texts + 1] = { diag.message, " " .. hi[diag.severity] }
        end

        local winid = vim.diagnostic.open_float(nil, {
          focus = false,
          scope = "line",
          border = "rounded",
          header = "",
          -- prefix = "󱓻 ",
          prefix = "󱓻",
          source = virt_texts,

        })

        g.diagnostic_float_winid = winid
        g.diagnostic_float_line = curline

        -- Auto-close the float when cursor moves
        vim.api.nvim_create_autocmd("CursorMoved", {
          buffer = args.buf,
          callback = function()
            local current_line = vim.fn.line "."
            if g.diagnostic_float_winid and current_line ~= g.diagnostic_float_line then
              pcall(vim.api.nvim_win_close, g.diagnostic_float_winid, false)
              g.diagnostic_float_winid = nil
              g.diagnostic_float_line = nil
              -- Remove this autocmd after it fires once
              return true
            end
          end,
        })
      end, { buffer = args.buf, desc = "Show line diagnostics" })
    end
  end,
})

-- Override the virtual text diagnostic handler so that the most severe diagnostic is shown first.
local show_handler = vim.diagnostic.handlers.virtual_text.show
assert(show_handler)
local hide_handler = vim.diagnostic.handlers.virtual_text.hide
vim.diagnostic.handlers.virtual_text = {
  show = function(ns, bufnr, diagnostics, opts)
    table.sort(diagnostics, function(diag1, diag2)
      return diag1.severity > diag2.severity
    end)
    return show_handler(ns, bufnr, diagnostics, opts)
  end,
  hide = hide_handler,
}
