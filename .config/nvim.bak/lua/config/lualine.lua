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

local function spell_status()
  if vim.wo.spell then
    -- When spell is on, show the language name
    local lang = vim.bo.spelllang
    return "Spell:" .. lang
  else
    return "Spell:Off"
  end
end

require("lualine").setup {
  options = {
    theme = "rose-pine",
    component_separators = "|",
    section_separators = {},
    refresh = {
      statusline = 300,
      tabline = 300,
      winbar = 300,
    },
  },
  sections = {
    lualine_a = {
      { "mode", right_padding = 2 },
    },
    lualine_b = {
      { "branch" },
      { "diff" },
    },
    lualine_c = { { "filename", path = 1 },
    },
    lualine_x = {},
    lualine_y = {
      {
        "diagnostics",
        sources = { "nvim_diagnostic", "nvim_lsp" },
        sections = { "error", "warn", "info", "hint" },
        -- symbols = { error = "E", warn = "W", info = "I", hint = "H" },
        colored = true,           -- Displays diagnostics status in color if set to true.
        update_in_insert = false, -- Update diagnostics in insert mode.
        always_visible = false,   -- Show diagnostics even if there are none.
      },
    },
    lualine_z = {
      {
        -- lsp attaced
        showLsp,
        icon = "  LSP:",
      },
      spell_status,
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
