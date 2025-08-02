return {
  {
    "tpope/vim-fugitive",
    lazy = true,
    event = { "BufWinEnter" },
  },

  {
    "lewis6991/gitsigns.nvim",
    lazy = true,
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      require("gitsigns").setup {
        signs = {
          add = { text = " +" },
          change = { text = " ~" },
          delete = { text = " ~" },
          topdelete = { text = " ‾" },
          changedelete = { text = " │" },
          untracked = { text = " ┆" },
        },
        preview_config = { border = "rounded" },
        current_line_blame = false,
        current_line_blame_formatter = "<summary>, <author_time:%Y-%m-%d>-<author>",
      }
    end,
  },

  -- -- https://github.com/MariaSolOs/dotfiles/blob/main/.config/nvim/lua/plugins/diffview.lua
  -- {
  --   "sindrets/diffview.nvim",
  --   lazy = true,
  --   event = "VeryLazy",
  --   opts = function()
  --     -- local actions = require "diffview.actions"
  --     require("diffview.ui.panel").Panel.default_config_float.border = "rounded"
  --   end,
  -- },
}
