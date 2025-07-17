local icons = require("icon")

require("mason").setup({
  ui = {
    icons = icons.install,
  },
  registries = {
    "github:mason-org/mason-registry",
    "github:Crashdummyy/mason-registry",
  },
})
require("fidget").setup({ notification = { window = { winblend = 0 } } })

local ensure_installed = {}
local lsp_server = vim.tbl_keys(require("lsp.server") or {})
local lsp_extra = vim.tbl_values(require("lsp.formater_linter") or {})
local debugger_server = require("lsp.debugger") or {}

vim.list_extend(ensure_installed, lsp_server)
vim.list_extend(ensure_installed, debugger_server)
vim.list_extend(ensure_installed, lsp_extra)

require("mason-tool-installer").setup({
  ensure_installed = ensure_installed,
  run_on_start = true,
  start_delay = 100,
})

-- Setup servers
require("mason-lspconfig").setup({
  ensure_installed = {},
  automatic_installation = false,
  handlers = {
    function(server_name)
      local lspconfig = require("lspconfig")
      local ls_server_config = require("lsp.server") or {}
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
})

-- Setup Keymaps
vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("wtc/lsp_attach", { clear = true }),
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    if not client then
      return
    end

    local lsp_keymap = require("lsp.Keymaps")
    lsp_keymap.on_attach(client, args.buf)
  end,
})

-- Diagnostic configuration.
local diagnostic_icons = icons.diagnostics

vim.diagnostic.config({
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
    },
  },
})

-- Hover diagnostic
vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("wtc/diagnostic-hover", { clear = true }),
  callback = function(args)
    local bufnr = args.buf

    -- Skip oil buffers
    if vim.bo[bufnr].filetype == "oil" then
      return
    end

    -- Store float window info
    local diagnostic_float = {
      winid = nil,
      line = nil,
      timer = nil,
    }

    -- Function to show diagnostic float
    local function show_diagnostic_float()
      local curline = vim.api.nvim_win_get_cursor(0)[1]
      local diagnostics = vim.diagnostic.get(bufnr, { lnum = curline - 1 })

      if #diagnostics == 0 then
        return
      end

      -- Close existing float if any
      if diagnostic_float.winid and vim.api.nvim_win_is_valid(diagnostic_float.winid) then
        vim.api.nvim_win_close(diagnostic_float.winid, false)
      end

      -- Show new float
      local float_opts = {
        focus = false,
        scope = "line",
        border = "single", -- or "none" for no border
        style = "minimal",
        header = "",
        prefix = "󱓻 ",
        source = "always",
      }

      diagnostic_float.winid = vim.diagnostic.open_float(bufnr, float_opts)
      diagnostic_float.line = curline
    end

    -- Function to hide diagnostic float
    local function hide_diagnostic_float()
      if diagnostic_float.timer then
        diagnostic_float.timer:stop()
        diagnostic_float.timer = nil
      end

      if diagnostic_float.winid and vim.api.nvim_win_is_valid(diagnostic_float.winid) then
        vim.api.nvim_win_close(diagnostic_float.winid, false)
        diagnostic_float.winid = nil
        diagnostic_float.line = nil
      end
    end

    -- Show diagnostics on cursor hold (with delay)
    vim.api.nvim_create_autocmd("CursorHold", {
      buffer = bufnr,
      callback = function()
        show_diagnostic_float()
      end,
    })

    -- Hide diagnostics when cursor moves
    vim.api.nvim_create_autocmd("CursorMoved", {
      buffer = bufnr,
      callback = function()
        hide_diagnostic_float()
      end,
    })

    -- Hide when entering insert mode
    vim.api.nvim_create_autocmd("InsertEnter", {
      buffer = bufnr,
      callback = function()
        hide_diagnostic_float()
      end,
    })

    -- Manual keymap to show diagnostics immediately
    -- vim.keymap.set("n", "<leader>d", show_diagnostic_float, {
    --   buffer = bufnr,
    --   desc = "Show line diagnostics"
    -- })
  end,
})

-- Set updatetime for CursorHold (adjust as needed)
vim.opt.updatetime = 30

-- Your virtual text sorting is good, keep it
local show_handler = vim.diagnostic.handlers.virtual_text.show
local hide_handler = vim.diagnostic.handlers.virtual_text.hide

vim.diagnostic.handlers.virtual_text = {
  show = function(ns, bufnr, diagnostics, opts)
    table.sort(diagnostics, function(diag1, diag2)
      return diag1.severity < diag2.severity -- Fixed: < instead of >
    end)
    return show_handler(ns, bufnr, diagnostics, opts)
  end,
}
