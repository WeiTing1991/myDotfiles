require("mason").setup({
  ui = {
    icons = {
      package_installed = "✓",
      package_pending = "➜",
      package_uninstalled = "✗",
    },
  },
})

require("fidget").setup { notification = { window = { winblend = 0 } } }

local lsp_server = vim.tbl_keys(require("configs.lsp.configs.server") or {})

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
