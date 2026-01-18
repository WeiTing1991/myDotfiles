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
local lsp_server = require("lsp.server") or {}
local lsp_extra = require("lsp.formater_linter")


vim.list_extend(ensure_installed, vim.tbl_keys(lsp_server))
vim.list_extend(ensure_installed, vim.tbl_values(lsp_extra))

require("mason-tool-installer").setup({
  ensure_installed = ensure_installed,
  run_on_start = false,
  start_delay = 10,
})

local capabilities = vim.lsp.protocol.make_client_capabilities()
if pcall(require, "blink.cmp") then
  capabilities = require("blink.cmp").get_lsp_capabilities(capabilities)
end

capabilities.textDocument.foldingRange = {
  dynamicRegistration = false,
  lineFoldingOnly = true,
}

-- run MasonInstallAll
for server_name, opts in pairs(lsp_server) do
  -- local actual_server = server_name_map[server_name] or server_name
  local actual_server = server_name
-- 
  -- if actual_server == "jdtls" or actual_server == "ts_ls" then
  --   goto continue
  -- end
  local server_opts = vim.tbl_deep_extend("force", { capabilities = capabilities }, opts or {})
  vim.lsp.config(actual_server, server_opts)
  vim.lsp.enable(actual_server)
  -- vim.notify("LSP server '" .. actual_server .. "' not found in nvim-lspconfig", vim.log.levels.WARN)
end

-- This function resolves a difference between neovim nightly (version 0.11) and stable (version 0.10)
---@param client vim.lsp.Client
---@param method vim.lsp.protocol.Method
---@param bufnr? integer some lsp support methods only in specific files
---@return boolean
local function client_supports_method(client, method, bufnr)
  if vim.fn.has("nvim-0.11") == 1 then
    return client:supports_method(method, bufnr)
  else
    return client.supports_method(method, { bufnr = bufnr })
  end
end

-- Setup Keymaps
vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("wtc/lsp_attach", { clear = true }),
  callback = function(event) -- Changed from 'args' to 'event' for consistency
    local client = vim.lsp.get_client_by_id(event.data.client_id)
    if not client then
      return
    end

    local lsp_keymap = require("lsp.keymaps")
    lsp_keymap.on_attach(client, event.buf) -- Changed args.buf to event.buf

    -- Document highlight setup
    -- if
    --   client and client_supports_method(client, vim.lsp.protocol.Methods.textDocument_documentHighlight, event.buf)
    -- then
    --   local highlight_augroup = vim.api.nvim_create_augroup("kickstart-lsp-highlight", { clear = false })
    --   vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
    --     buffer = event.buf,
    --     group = highlight_augroup,
    --     callback = vim.lsp.buf.document_highlight,
    --   })
    --   vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
    --     buffer = event.buf,
    --     group = highlight_augroup,
    --     callback = vim.lsp.buf.clear_references,
    --   })
    --   vim.api.nvim_create_autocmd("LspDetach", {
    --     group = vim.api.nvim_create_augroup("kickstart-lsp-detach", { clear = true }),
    --     callback = function(event2)
    --       vim.lsp.buf.clear_references()
    --       vim.api.nvim_clear_autocmds({ group = "kickstart-lsp-highlight", buffer = event2.buf })
    --     end,
    --   })
    -- end
  end,
})

-- -- TODO: make a plugin
-- -- Powerful diagnostic plugin:
-- local show_handler = vim.diagnostic.handlers.virtual_text.show
-- local hide_handler = vim.diagnostic.handlers.virtual_text.hide
--
-- vim.diagnostic.handlers.virtual_text = {
--   show = function(ns, bufnr, diagnostics, opts)
--     table.sort(diagnostics, function(diag1, diag2)
--       return diag1.severity < diag2.severity
--     end)
--     return show_handler(ns, bufnr, diagnostics, opts)
--   end,
-- }
--
-- vim.api.nvim_create_autocmd("LspAttach", {
--   group = vim.api.nvim_create_augroup("wtc/diagnostic-hover", { clear = true }),
--   callback = function(args)
--     local bufnr = args.buf
--     local diagnostic_icons = icons.diagnostics
--     -- Skip some buffers
--     if vim.bo[bufnr].filetype == "oil" then
--       return
--     end
--     -- Store float window info
--     local diagnostic_float = {
--       winid = nil,
--       line = nil,
--     }
--     -- Configure diagnostics - no virtual text by default
--     vim.diagnostic.config({
--       signs = {
--         text = {
--           [vim.diagnostic.severity.ERROR] = diagnostic_icons.Error,
--           [vim.diagnostic.severity.WARN] = diagnostic_icons.Warn,
--           [vim.diagnostic.severity.INFO] = diagnostic_icons.Info,
--           [vim.diagnostic.severity.HINT] = diagnostic_icons.Hint,
--         },
--         texthl = {
--           [vim.diagnostic.severity.ERROR] = "DiagnosticSignError",
--           [vim.diagnostic.severity.WARN] = "DiagnosticSignWarn",
--           [vim.diagnostic.severity.INFO] = "DiagnosticSignInfo",
--           [vim.diagnostic.severity.HINT] = "DiagnosticSignHint",
--         },
--         numhl = {
--           [vim.diagnostic.severity.ERROR] = "DiagnosticSignError",
--           [vim.diagnostic.severity.WARN] = "DiagnosticSignWarn",
--           [vim.diagnostic.severity.INFO] = "DiagnosticSignInfo",
--           [vim.diagnostic.severity.HINT] = "DiagnosticSignHint",
--         },
--       },
--       virtual_text = false,
--       underline = true,
--       update_in_insert = true,
--       float = false,
--     })
--     vim.api.nvim_set_hl(0, "DiagnosticUnderlineHint", {
--       underline = false,
--       underdotted = true,
--       sp = "#61AFEF",
--     })
--     -- Create namespace for our custom virtual text
--     local ns_id = vim.api.nvim_create_namespace("diagnostic_current_line")
--     local last_line = nil
--
--     -- Function to get diagnostic highlight group and icon
--     local function get_diagnostic_info(severity)
--       if severity == vim.diagnostic.severity.ERROR then
--         return "DiagnosticVirtualTextError", diagnostic_icons.Error
--       elseif severity == vim.diagnostic.severity.WARN then
--         return "DiagnosticVirtualTextWarn", diagnostic_icons.Warn
--       elseif severity == vim.diagnostic.severity.INFO then
--         return "DiagnosticVirtualTextInfo", diagnostic_icons.Info
--       elseif severity == vim.diagnostic.severity.HINT then
--         return "DiagnosticVirtualTextHint", diagnostic_icons.Hint
--       else
--         return "DiagnosticVirtualTextError", diagnostic_icons.Error
--       end
--     end
--
--     -- Function to show virtual text on current line only
--     local function update_virtual_text()
--       local curline = vim.api.nvim_win_get_cursor(0)[1]
--       -- Always clear previous virtual text first
--       vim.api.nvim_buf_clear_namespace(bufnr, ns_id, 0, -1)
--       -- Only proceed if line has changed
--       if last_line == curline then
--         return
--       end
--       last_line = curline
--       local diagnostics = vim.diagnostic.get(bufnr, { lnum = curline - 1 })
--       -- Show virtual text only for current line if it has diagnostics
--       if #diagnostics > 0 then
--         local diag = diagnostics[1] -- Show first diagnostic
--         local hl_group, icon = get_diagnostic_info(diag.severity)
--
--         vim.api.nvim_buf_set_extmark(bufnr, ns_id, curline - 1, -1, {
--           virt_text = {
--             { icon, hl_group }, -- Use matching icon with proper color
--             { " " .. diag.message, hl_group }, -- Message with proper color
--           },
--           virt_text_pos = "eol",
--         })
--       end
--     end
--     -- Function to hide virtual text
--     local function hide_virtual_text()
--       vim.api.nvim_buf_clear_namespace(bufnr, ns_id, 0, -1)
--       last_line = nil
--     end
--     -- Function to show diagnostic float (for Ctrl+K)
--     local function show_diagnostic_float()
--       local curline = vim.api.nvim_win_get_cursor(0)[1]
--       local diagnostics = vim.diagnostic.get(bufnr, { lnum = curline - 1 })
--       if #diagnostics == 0 then
--         return
--       end
--       -- Hide virtual text when showing float
--       hide_virtual_text()
--       -- Close existing float if any
--       if diagnostic_float.winid and vim.api.nvim_win_is_valid(diagnostic_float.winid) then
--         vim.api.nvim_win_close(diagnostic_float.winid, false)
--       end
--       -- Show new float
--       local float_opts = {
--         focus = false,
--         scope = "line",
--         border = "single",
--         style = "minimal",
--         header = "",
--         prefix = "󱓻 ",
--         source = "if_many",
--         wrap = true,
--         max_width = 50,
--       }
--       diagnostic_float.winid = vim.diagnostic.open_float(bufnr, float_opts)
--       diagnostic_float.line = curline
--     end
--     -- Function to hide diagnostic float and return to normal
--     local function hide_diagnostic_float()
--       if diagnostic_float.winid and vim.api.nvim_win_is_valid(diagnostic_float.winid) then
--         vim.api.nvim_win_close(diagnostic_float.winid, false)
--         diagnostic_float.winid = nil
--         diagnostic_float.line = nil
--       end
--       -- Return to normal virtual text behavior
--       update_virtual_text()
--     end
--     -- Update virtual text when cursor moves
--     vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
--       buffer = bufnr,
--       callback = function()
--         -- Only show virtual text if no float is currently shown
--         if not (diagnostic_float.winid and vim.api.nvim_win_is_valid(diagnostic_float.winid)) then
--           update_virtual_text()
--         end
--       end,
--     })
--     -- Hide virtual text when entering insert mode
--     vim.api.nvim_create_autocmd("InsertEnter", {
--       buffer = bufnr,
--       callback = function()
--         hide_virtual_text()
--         if diagnostic_float.winid and vim.api.nvim_win_is_valid(diagnostic_float.winid) then
--           vim.api.nvim_win_close(diagnostic_float.winid, false)
--           diagnostic_float.winid = nil
--         end
--       end,
--     })
--     -- Show virtual text when leaving insert mode
--     vim.api.nvim_create_autocmd("InsertLeave", {
--       buffer = bufnr,
--       callback = function()
--         vim.defer_fn(update_virtual_text, 100)
--       end,
--     })
--     -- Hide virtual text when buffer loses focus
--     vim.api.nvim_create_autocmd("BufLeave", {
--       buffer = bufnr,
--       callback = hide_virtual_text,
--     })
--     -- Ctrl+K to show hover float
--     vim.keymap.set("n", "<A-k>", function()
--       show_diagnostic_float()
--       -- Auto-hide float after 1.5 seconds and return to normal
--       vim.defer_fn(function()
--         hide_diagnostic_float()
--       end, 1500)
--     end, {
--       buffer = bufnr,
--       desc = "Show diagnostic hover",
--     })
--     -- Press Escape to hide float and return to normal
--     vim.keymap.set("n", "<Esc>", function()
--       hide_diagnostic_float()
--     end, { buffer = bufnr }) -- Initial call to show virtual text
--     vim.defer_fn(update_virtual_text, 200)
--   end,
-- })
