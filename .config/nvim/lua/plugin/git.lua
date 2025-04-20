return {

  -- https://github.com/MariaSolOs/dotfiles/blob/main/.config/nvim/lua/plugins/diffview.lua
  {
    "sindrets/diffview.nvim",
    lazy = true,
    event = "VeryLazy",
    opts = function()
      local actions = require "diffview.actions"
      require("diffview.ui.panel").Panel.default_config_float.border = "rounded"
    end,
  },

  {
    "lewis6991/gitsigns.nvim",
    lazy = true,
    event = { 'BufReadPre', 'BufNewFile' },
    config = function()
      require("gitsigns").setup {
        signs = {
          add = { text = "+" },
          change = { text = "~" },
          delete = { text = "_" },
          topdelete = { text = "‾" },
          changedelete = { text = "│" },
          untracked = { text = "┆" },
        },
        preview_config = { border = 'rounded' },
        current_line_blame = false,
        -- Toggle with `:Gitsigns toggle_current_line_blame`
        -- current_line_blame_formatter = "<author>, <author_time:%Y-%m-%d>-<summary>",
        current_line_blame_formatter = "<summary>, <author_time:%Y-%m-%d>-<author>",
      }
    end,
  },
}
