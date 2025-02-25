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

      if server_name == "ruff_lsp" then
        if server.server_capabilities == nil then
          server.server_capabilities = {}
        end
        server.server_capabilities.hoverProvider = false
        server.server_capabilities.documentHighlightProvider = false
      end

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

-- Logging for debugging
vim.lsp.set_log_level "debug"
vim.lsp.handlers["$/progress"] = function(_, result, ctx)
  print("LSP progress:", vim.inspect(result))
end

-- Hover diagnostic
local highlight_augroup = vim.api.nvim_create_augroup("diagnostic-hover", { clear = false })
local ns = vim.api.nvim_create_namespace "CurlineDiag"

-- NOT WORKING
local diagnostic_enabled = true -- Track if diagnostics are enabled

local function toggle_diagnostics()
  diagnostic_enabled = not diagnostic_enabled
  if diagnostic_enabled then
    print "Diagnostics Hover Enabled"
  else
    vim.api.nvim_clear_autocmds { group = highlight_augroup }
    print "Diagnostics Hover Disabled"
  end
end

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

-- vim.api.nvim_set_keymap("n", "<leader>tt", toggle_diagnostics(), { noremap = true, silent = true })

-- Change signs for LSP diagnostics
local signs = {
  Error = " ",
  Warn = " ",
  Info = " ",
  Hint = "󰠠 ",
}

local function format_diagnostic(diagnostic)
  local icon = signs.Error
  if diagnostic.severity == vim.diagnostic.severity.WARN then
    icon = signs.Warn
  elseif diagnostic.severity == vim.diagnostic.severity.INFO then
    icon = signs.Info
  elseif diagnostic.severity == vim.diagnostic.severity.HINT then
    icon = signs.Hint
  end

  local message = string.format("%s %s", icon, diagnostic.message)
  if diagnostic.code and diagnostic.source then
    message = string.format("%s [%s][%s] %s", icon, diagnostic.source, diagnostic.code, diagnostic.message)
  elseif diagnostic.code or diagnostic.source then
    message = string.format("%s [%s] %s", icon, diagnostic.code or diagnostic.source, diagnostic.message)
  end

  return message .. " "
end

for type, icon in pairs(signs) do
  vim.fn.sign_define(
    "DiagnosticSign" .. type,
    { text = icon, texthl = "DiagnosticSign" .. type, numhl = "DiagnosticSign" .. type }
  )
end

-- diagnostic
vim.diagnostic.config {
  underline = true,
  update_in_insert = true,
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
