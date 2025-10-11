local M = {}

M.base46 = {
    theme = "darcula-dark",
    hl_add = {
      WinSeparatorFocused = {  bold = true }, -- Bright blue for focused
    },
    integrations = {},
    hl_override = { },
    changed_themes = {},
    transparency = true,
    theme_toggle = { "darcula-dark", "one_light" },
}

M.ui = {
    cmp = {
      icons_left = false, -- only for non-atom styles!
      style = "default", -- default/flat_light/flat_dark/atom/atom_colored
      abbr_maxwidth = 60,
      -- for tailwind, css lsp etc
      format_colors = { lsp = true, icon = "󱓻" },
    },
    telescope = { style = "borderless" },
    statusline = { enabled = false },
    tabufline = {
      enabled = false,
      lazyload = true,
      order = { "treeOffset", "buffers", "tabs", "btns" },
      modules = nil,
      bufwidth = 21,
    },
  }

M.nvdash = {
  load_on_startup = false,
}

M.term = {
  base46_colors = true,
  winopts = { number = false, relativenumber = false },
  sizes = { sp = 0.3, vsp = 0.2, ["bo sp"] = 0.3, ["bo vsp"] = 0.2 },
  float = {
    relative = "editor",
    row = 0.3,
    col = 0.25,
    width = 0.5,
    height = 0.4,
    border = "single",
  },
}

M.lsp = { signature = true }

M.cheatsheet = {
  theme = "grid", -- simple/grid
  excluded_groups = { "terminal (t)", "autopairs", "Nvim", "Opens" },
}

M.mason = { pkgs = {}, skip = {} }

M.colorify = {
    enabled = true,
    mode = "virtual", -- fg, bg, virtual
    virt_text = "󱓻 ",
    highlight = { hex = true, lspvars = true },
}

return M
