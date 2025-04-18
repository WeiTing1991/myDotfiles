require("mason").setup {
  ui = {
    icons = {
      package_installed = "✓",
      package_pending = "➜",
      package_uninstalled = "✗",
    },
  },
}

require("fidget").setup { notification = { window = { winblend = 0 } } }

local lspconfig = require "lspconfig"
local lsp_server = vim.tbl_keys(require "configs.lsp.configs.server" or {})

local lsp_lint_fomater = vim.tbl_values(require "configs.lsp.configs.extra" or {})
-- local dap_server = vim.tbl_values(require "lsp.dap-server")
local ensure_installed = {}

-- fomater
for _, value in ipairs(lsp_lint_fomater) do
  table.insert(ensure_installed, value)
end

-- dap server
-- for _, value in ipairs(dap_server) do
--   table.insert(ensure_installed, value)
-- end

require("mason-tool-installer").setup {
  ensure_installed = ensure_installed,
  run_on_start = true,
}

require("lspconfig").protols.setup {}

require("mason-lspconfig").setup {
  ensure_installed = lsp_server,
  automatic_installation = false,
  handlers = {
    function(server_name)
      local server = lsp_server[server_name] or {}

      -- Useful when disabling
      -- dissable typscript/javaserver attach here
      if server_name == "jdtls" or server_name == "ts_ls" then
        return
      end

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
      capabilities = vim.tbl_deep_extend("force", capabilities, require("blink.cmp").get_lsp_capabilities(capabilities))

      -- Use the extended capabilities
      server.capabilities = capabilities
      lspconfig[server_name].setup(server)

      -- for fold
      -- capabilities.textDocument.foldingRange = {
      --   dynamicRegistration = false,
      --   lineFoldingOnly = true,
      -- }
    end,
  },
}

-- Hover diagnostic
local highlight_augroup = vim.api.nvim_create_augroup("diagnostic-hover", { clear = false })
local ns = vim.api.nvim_create_namespace "CurlineDiag"

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local g = g or {}
    g.diagnostic_float_winid = nil
    g.diagnostic_float_line = nil

    -- Ensure the buffer exists and has diagnostics
    if vim.api.nvim_buf_is_valid(args.buf) then
      -- Check if the buffer is of a specific type (e.g., 'oil') and skip it if necessary
      if vim.bo[args.buf].filetype == "oil" then
        return
      end

      -- Set up a keymap to show diagnostics float
      vim.keymap.set("n", "<C-m>", function()
        local ns = vim.api.nvim_create_namespace "diagnostics_ns"
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
          prefix = "󱓻 ",
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

-- diagnostic
vim.diagnostic.config {
  underline = true,
  update_in_insert = false,
  severity_sort = true,
  sings = true,
  virtual_text = false,
  -- float = {
  --   -- border = user_config.border,
  --   focusable = false,
  --   -- header = { icons.debug .. ' Diagnostics:', 'DiagnosticInfo' },
  --   scope = 'line',
  --   suffix = '',
  --   source = false,
  --   format = format_diagnostic,
  -- },
  -- virtual_text = {
  --   prefix = '',
  --   spacing = 2,
  --   source = false,
  --   severity = {
  --     min = vim.diagnostic.severity.HINT,
  --   },
  --   format = format_diagnostic,
  -- },
}
