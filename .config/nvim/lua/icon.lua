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
  package_installed = "✓",
  package_pending = "➜",
  package_uninstalled = "✗",
}

--- LSP symbol kinds.
M.symbol_kinds = {
  Array = "󰅪",
  Class = "",
  Color = "󰏘",
  Constant = "󰏿",
  Constructor = "",
  Enum = "",
  EnumMember = "",
  Event = "",
  Field = "󰜢",
  File = "󰈙",
  Folder = "󰉋",
  Function = "󰆧",
  Interface = "",
  Keyword = "󰌋",
  Method = "󰆧",
  Module = "",
  Operator = "󰆕",
  Property = "󰜢",
  Reference = "󰈇",
  Snippet = "",
  Struct = "",
  Text = "",
  TypeParameter = "",
  Unit = "",
  Value = "",
  Variable = "󰀫",
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
    git = { unmerged = "" },
  },
}

--- Shared icons that don't really fit into a category.
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
