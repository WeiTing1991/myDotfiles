-- NOTE: https://github.com/tamton-aquib/staline.nvim
-- https://github.com/tamton-aquib/staline.nvim/blob/main/lua/staline/init.lua

local function spell_check()
  if vim.wo.spell then
    return " " .. vim.opt.spelllang:get()[1] .. " "
  else
    return " "
  end
end

local function copilot_status()
  if vim.b.copilot_suggestion_auto_trigger then
    return " " .. "copilot"
  else
    return " "
  end
end

local function indent_style()
  local style = vim.opt.expandtab:get() and "" or "⇥"
  local tab_width = vim.opt.shiftwidth:get()
  return string.format("%s (%d)", style, tab_width)
end

require("staline").setup {
  defaults = {
    expand_null_ls = false, -- This expands out all the null-ls sources to be shown
    -- left_separator = "",
    -- right_separator = "",
    left_separator = "",
    right_separator = "",

    full_path = false,
    branch_symbol = " ",
    -- bg = "#303030",
    -- fg = "#000000", -- Foreground text color.
    inactive_color = "#303030",
    inactive_bgcolor = "none",
    true_colors = true, -- true lsp colors.
    font_active = "bold", -- "bold", "italic", "bold,italic", etc
    line_column = " [%l/%L] :%c",
  },

  mode_colors = {},
  mode_icons = {},
  sections = {
    left = { "- ", "-mode", "left_sep_double", "file_name", "branch" },
    mid = {},
    right = { copilot_status, spell_check, "lsp", indent_style, "right_sep_double", "-cwd" },
  },

  inactive_sections = {
    -- left = { "branch" },
    -- mid = { spell_check },
    -- right = {},
  },

  special_table = {
    lazy = { "LAZY", "  " },
    mason = { "MASON", "  " },
  },

  lsp_symbols = {
    Error = " ",
    Warn = " ",
    Info = " ",
    Hint = "",
  },
}
