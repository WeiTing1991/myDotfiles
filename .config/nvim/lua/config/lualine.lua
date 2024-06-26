--	local lazy_status = require("lazy.status") -- to configure lazy pending updates count

local function showLsp()
  local msg = "NoActiveLsp"
  local buf_ft = vim.api.nvim_buf_get_option(0, "filetype")
  local clients = vim.lsp.get_active_clients()

  if next(clients) == nil or buf_ft == "java" then
    return msg
  end

  for _, client in ipairs(clients) do
    local filetypes = client.config.filetypes
    if vim.tbl_contains(filetypes, buf_ft) then
      if filetypes and client.name ~= nil and client.name ~= "null-ls" then
        return client.name
      end
    end
  end
  return msg
end

require("lualine").setup {
  options = {
    theme = "codedark",
    component_separators = "|",
    section_separators = { left = "", right = "" },
    refresh = {
      statusline = 500,
      tabline = 500,
      winbar = 500,
    },
  },
  sections = {
    lualine_a = {
      { "mode", right_padding = 2 },
      { "filename", path = 0 },
    },
    lualine_b = { "branch", "diff" },
    lualine_c = {},
    lualine_x = {},
    lualine_y = {
      {
        -- lsp attaced
        showLsp,
        icon = "  LSP:",
        color = { fg = "#31748f"},
      },
      {
        "diagnostics",
        sources = { "nvim_diagnostic", "nvim_lsp" },
        sections = { "error", "warn", "info", "hint" },
        diagnostics_color = {
          -- Same values as the general color option can be used here.
          error = "DiagnosticError", -- Changes diagnostics' error color.
          warn = "DiagnosticWarn", -- Changes diagnostics' warn color.
          info = "DiagnosticInfo", -- Changes diagnostics' info color.
          hint = "DiagnosticHint", -- Changes diagnostics' hint color.
        },
        symbols = { error = "E", warn = "W", info = "I", hint = "H" },
        colored = true, -- Displays diagnostics status in color if set to true.
        update_in_insert = false, -- Update diagnostics in insert mode.
        always_visible = false, -- Show diagnostics even if there are none.
      },
    },
    lualine_z = {
      "progress",
      { "location", left_padding = 2 },
    },
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {},
    lualine_x = {},
    lualine_y = {},
    lualine_z = {},
  },
}
