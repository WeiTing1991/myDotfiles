-- spell statues
local function spell_check()
  if vim.wo.spell then
    return "󰓆 " .. vim.opt.spelllang:get()[1] .. "Spell"
  else
    return ""
  end
end

-- copilot statues
local function copilot_status()
  if not vim.b.copilot_suggestion_auto_trigger then
    return " "
  else
    return " "
  end
end

-- indent statues
local function indent_style()
  local sytle_string = vim.opt.expandtab:get() and "space" or "tab"
  local style = vim.opt.expandtab:get() and "⇥ " or " "
  local tab_width = vim.opt.shiftwidth:get()
  return string.format("%s%s(%d)", style, sytle_string, tab_width)
end

require("lualine").setup({
  options = {
    icons_enabled = true,
    theme = "auto",
    component_separators = { left = "", right = "" },
    section_separators = { left = "", right = "" },
    disabled_filetypes = {
      statusline = {},
      winbar = {},
    },
    ignore_focus = {},
    always_divide_middle = true,
    always_show_tabline = false,
    globalstatus = true, -- Recommended for modern nvim
    refresh = {
      statusline = 1000,
      tabline = 1000,
      winbar = 1000,
      refresh_time = 16, -- ~60fps
      events = {
        "WinEnter",
        "BufEnter",
        "BufWritePost",
        "SessionLoadPost",
        "FileChangedShellPost",
        "VimResized",
        "Filetype",
        "CursorMoved",
        "CursorMovedI",
        "ModeChanged",
      },
    },
  },
  sections = {
    lualine_a = { "mode" },
    lualine_b = { "branch", "diff", "diagnostics" },
    lualine_c = { "filename" },
    lualine_x = { "encoding" },
    lualine_y = { indent_style, spell_check, copilot_status },
    lualine_z = { "location" },
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = { "filename" },
    lualine_x = { "location" },
    lualine_y = {},
    lualine_z = {},
  },
  tabline = {},
  winbar = {},
  inactive_winbar = {},
  extensions = {},
})
