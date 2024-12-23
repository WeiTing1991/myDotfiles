local M = {

  base46 = {
    theme = "nord", -- default theme
    hl_add = {},
    hl_override = {},
    integrations = {},
    changed_themes = {},
    transparency = true,
    theme_toggle = { "nord", "Nano_Light" },
  },

  ui = {
    cmp = {
      icons_left = false, -- only for non-atom styles!
      lspkind_text = true,
      style = "default", -- default/flat_light/flat_dark/atom/atom_colored
      format_colors = {
        tailwind = false, -- will work for css lsp too
        icon = "󱓻",
      },
    },

    telescope = { style = "borderless" }, -- borderless / bordered

    statusline = {
      enabled = true,
      theme = "vscode_colored", -- default/vscode/vscode_colored/minimal
      -- default/round/block/arrow separators work only for default statusline theme
      -- round and block will work for minimal theme only
      separator_style = "default",
      -- order = { "mode", "file", "git", "%=", "lsp_msg", "%=", "diagnostics", "lsp", "cwd", "cursor" },
      order = { "mode", "file", "git", "%=", "lsp_msg", "%=", "lsp", "spell_check", "cursor", "cwd"},
      modules = {
        -- f = "%F",

        spell_check = function()
          if vim.wo.spell then
            return "SP: [" .. vim.opt.spelllang:get()[1] .. "]"
          else
            return "SP: Off"
          end
        end,
      },
    },

    -- lazyload it when there are 1+ buffers
    tabufline = {
      enabled = false,
      lazyload = true,
      order = { "treeOffset", "buffers", "tabs", "btns" },
      modules = nil,
    },
  },

  nvdash = {
    load_on_startup = true,
    header = {

      "                          ",
      "    ██╗    ██╗████████╗   ",
      "    ██║    ██║╚══██╔══╝   ",
      "    ██║ █╗ ██║   ██║      ",
      "    ██║███╗██║   ██║      ",
      "    ╚███╔███╔╝   ██║      ",
      "     ╚══╝╚══╝    ╚═╝      ",
      "                          ",
      "    Powered By  eovim   ",
      "                          ",
    },

    buttons = {
      { txt = "  Find File", keys = "Space f", cmd = "Telescope find_files" },
      { txt = "  Recent Files", keys = "Space fo", cmd = "Telescope oldfiles" },
      { txt = "󰈭  Find Word", keys = "Space fw", cmd = "Telescope live_grep" },
      -- { txt = "󱥚  Themes", keys = "th", cmd = ":lua require('nvchad.themes').open()" },
      { txt = "  Mappings", keys = "Space fk", cmd = "NvCheatsheet" },

      { txt = "─", hl = "NvDashFooter", no_gap = true, rep = true },
      {
        txt = function()
          local stats = require("lazy").stats()
          local ms = math.floor(stats.startuptime) .. " ms"
          return "  Loaded " .. stats.loaded .. "/" .. stats.count .. " plugins in " .. ms
        end,
        hl = "NvDashFooter",
        no_gap = true,
      },
      { txt = "─", hl = "NvDashFooter", no_gap = true, rep = true },

      -- {
      --   txt = function()
      --     local dir = vim.loop.cwd()
      --     return dir
      --   end,
      --   no_gap = true,
      -- },

    },
  },

  term = {
    winopts = { number = false, relativenumber = false },
    sizes = { sp = 0.3, vsp = 0.2, ["bo sp"] = 0.3, ["bo vsp"] = 0.2 },
    float = {
      relative = "editor",
      row = 0.1,
      col = 0.1,
      width = 0.8,
      height = 0.6,
      border = "single",
    },
  },

  lsp = { signature = true },

  cheatsheet = {
    theme = "grid", -- simple/grid
    excluded_groups = { "terminal (t)", "autopairs", "Nvim", "Opens" }, -- can add group name or with mode
  },

  mason = { pkgs = {}, skip = {} },

  colorify = {
    enabled = true,
    mode = "virtual", -- fg, bg, virtual
    virt_text = "󱓻 ",
    highlight = { hex = true, lspvars = true },
  },
}

return M
