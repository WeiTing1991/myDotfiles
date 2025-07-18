-- NOTE: https://github.com/tamton-aquib/staline.nvim
local icon = require("icon")

-- spell statues
local function spell_check()
  if vim.wo.spell then
    return "  " .. vim.opt.spelllang:get()[1] .. "Spell"
  else
    return "  "
  end
end

-- copilot statues
local function copilot_status()
  if not vim.b.copilot_suggestion_auto_trigger then
    return "   "
  else
    return "   "
  end
end
-- indent statues
local function indent_style()
  local sytle_string = vim.opt.expandtab:get() and "space" or "tab"
  local style = vim.opt.expandtab:get() and "⇥" or ""
  local tab_width = vim.opt.shiftwidth:get()
  return string.format("%s %s (%d)", style, sytle_string, tab_width)
end

require("staline").setup({
  defaults = {
    expand_null_ls = false, -- This expands out all the null-ls sources to be shown
    -- left_separator = "",
    -- right_separator = "",
    left_separator = "",
    right_separator = "",
    full_path = true,
    branch_symbol = " ",
    true_colors = true,
    font_active = "bold,italic", -- "bold", "italic", "bold,italic", etc
    line_column = " [%l/%L] :%c",
  },
  mode_colors = {},
  mode_icons = {},
  sections = {
    left = { "- ", "mode", "right_sep_double","-file_name", "left_sep_double", "branch" },
    mid = { "lsp" },
    right = { spell_check, copilot_status, indent_style, "right_sep_double", "-line_column","left_sep_double", "cwd", "- " },
  },
  inactive_sections = {
    -- left = { "lsp" },
    -- mid = { spell_check },
    -- right = {},
  },

  special_table = {
    lazy = { "LAZY", "  " },
    mason = { "MASON", "  " },
    snacks_dashboard = { "HOME", "" },
  },
  lsp_symbols = icon.diagnostics,
})
