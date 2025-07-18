-- inspire by https://github.com/MariaSolOs/
local M = {}

--- Diagnostic severities.
M.diagnostics = {
  Error = " ",
  Warn = " ",
  Info = " ",
  Hint = "󰠠 ",
}

--- For folding.
M.arrows = {
  right = "",
  left = "",
  up = "",
  down = "",
}

--- For install
M.install = {
  package_installed = "●",
  package_pending = "➜",
  package_uninstalled = "○",
}

--- LSP symbol kinds.
M.symbol_kinds = {
  Array = " ",
  Boolean = "󰨙 ",
  Class = " ",
  Codeium = "󰘦 ",
  Color = " ",
  Control = " ",
  Collapsed = " ",
  Constant = "󰏿 ",
  Constructor = " ",
  Copilot = " ",
  Enum = " ",
  EnumMember = " ",
  Event = " ",
  Field = " ",
  File = " ",
  Folder = " ",
  Function = "󰊕 ",
  Interface = " ",
  Key = " ",
  Keyword = " ",
  Method = "󰊕 ",
  Module = " ",
  Namespace = "󰦮 ",
  Null = " ",
  Number = "󰎠 ",
  Object = " ",
  Operator = " ",
  Package = " ",
  Property = " ",
  Reference = " ",
  Snippet = "󱄽 ",
  String = " ",
  Struct = "󰆼 ",
  Supermaven = " ",
  TabNine = "󰏚 ",
  Text = " ",
  TypeParameter = " ",
  Unit = " ",
  Value = " ",
  Variable = "󰀫 ",
}

M.tree = {
  glyphs = {
    default = "󰈚",
    folder = {
      default = "",
      empty = "",
      empty_open = "",
      open = "",
      symlink = "@",
    },
    git = {
      unstaged ="○",
      staged ="●",
      ignored = "",
    },
  },
}

M.misc = {
  bug = "",
  ellipsis = "…",
  git = "",
  search = "",
  vertical_bar = "▏",
  dashed_bar = "┊ ",
  scrollchars = { "┃", "" },
}

return M
