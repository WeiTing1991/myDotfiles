require("transparent").clear_prefix "trouble"
require("transparent").clear_prefix "NvimTree"
require("transparent").clear_prefix "oil"

require("transparent").setup {
  -- table: default groups
  groups = {
    "Normal",
    "NormalNC",
    "Comment",
    "Constant",
    "Special",
    "Identifier",
    "Statement",
    "PreProc",
    "Type",
    "Underlined",
    "Todo",
    "String",
    "Function",
    "Conditional",
    "Repeat",
    "Operator",
    "Structure",
    "LineNr",
    "NonText",
    "SignColumn",
    "CursorLine",
    "CursorLineNr",
    "StatusLine",
    "StatusLineNC",
    "EndOfBuffer",
  },
  -- table: additional groups that should be cleared
  extra_groups = {
    "TroubleNormal",
    "FloatBorder",
  },
  -- table: groups you don't want to clear
  exclude_groups = {
    "Todo",
    "Comment",
    "StatusLine",
    "StatusLineNC",
    "CursorLine",
    "CursorLineNr",
    "CnrsorColumn",
  },
  -- function: code to be executed after highlight groups are cleared
  -- Also the user event "TransparentClear" will be triggered
  on_clear = function() end,
}

vim.g.transparent_groups = vim.list_extend(vim.g.transparent_groups or {}, { "ExtraGroup" })
