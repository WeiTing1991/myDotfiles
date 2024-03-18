-- https://github.com/EdenEast/nightfox.nvim?tab=readme-ov-file
-- https://github.com/EdenEast/nightfox.nvim/blob/main/usage.md#module
local opts = {
  options = {
    transparent = true, -- Disable setting background
    terminal_colors = true, -- Set terminal colors (vim.g.terminal_color_*) used in `:terminal`
    dim_inactive = false, -- Non focused panes set to alternative background
    module_default = false, -- Default enable value for modules
  },

  inverse = { -- Inverse highlight for different types
    match_paren = false,
    visual = false,
    search = false,
  },
  modules = { -- List of various plugins and additional options
  },
  palettes = {},
  specs = {},
  groups = {
    -- help highlight_group
    -- https://github.com/EdenEast/nightfox.nvim/blob/main/lua/nightfox/palette/carbonfox.lua
    all = {
      Visual = { bg = "#60728a" },
      IncSearch = { bg = "palette.cyan" },
    },
  },
}

require("nightfox").setup(opts)
--require("nightfox").load()
