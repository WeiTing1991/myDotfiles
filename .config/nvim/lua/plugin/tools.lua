return {
  -- -------------------------------- TOOL  -------------------------------------------------------
  -- tmux navigator
  {
    "christoomey/vim-tmux-navigator",
    lazy = true,
    event = "BufEnter",
    cmd = {
      "TmuxNavigateLeft",
      "TmuxNavigateDown",
      "TmuxNavigateUp",
      "TmuxNavigateRight",
      "TmuxNavigatePrevious",
    },
    keys = {
      { "<c-h>",  "<cmd><C-U>TmuxNavigateLeft<cr>" },
      { "<c-j>",  "<cmd><C-U>TmuxNavigateDown<cr>" },
      { "<c-k>",  "<cmd><C-U>TmuxNavigateUp<cr>" },
      { "<c-l>",  "<cmd><C-U>TmuxNavigateRight<cr>" },
      { "<c-\\>", "<cmd><C-U>TmuxNavigatePrevious<cr>" },
    },
  },

  -- NOTE: https://www.lazyvim.org/plugins/editor#troublenvim
  {
    "folke/trouble.nvim",
    enabled = false,
    lazy = true,
    event = "VeryLazy",
    opts = {},
    cmd = "Trouble",
  },

  -- git tools
  {
    "lewis6991/gitsigns.nvim",
    lazy = true,
    event = "VeryLazy",
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
        current_line_blame = false, -- Toggle with `:Gitsigns toggle_current_line_blame`
        -- current_line_blame_formatter = "<author>, <author_time:%Y-%m-%d>-<summary>",
        current_line_blame_formatter = "<summary>, <author_time:%Y-%m-%d>-<author>",
      }
    end,
  },

  {
    "tpope/vim-fugitive",
    lazy = true,
    event = "VeryLazy",
  },

  {
    "sindrets/diffview.nvim",
    lazy = true,
    event = "VeryLazy",
  },

  -- ai tools
  {
    "zbirenbaum/copilot.lua",
    lazy = true,
    cmd = "Copilot",
    event = "InsertEnter",
    config = function()
      require "configs.copilot"
    end,
  },

}
