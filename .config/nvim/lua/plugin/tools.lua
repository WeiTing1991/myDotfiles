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

  {
    "NeogitOrg/neogit",
    lazy = true,
    event = "VeryLazy",
    dependencies = {
      "nvim-lua/plenary.nvim",  -- required
      "sindrets/diffview.nvim", -- optional - Diff integration
      "ibhagwan/fzf-lua",       -- optional
    },
    config = true
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

  {
    "CopilotC-Nvim/CopilotChat.nvim",
    lazy = false,
    -- event = "VeryLazy",
    dependencies = {
      { "zbirenbaum/copilot.lua" },
      { "nvim-lua/plenary.nvim", branch = "master" }, -- for curl, log and async functions
    },
    -- build = "make tiktoken", -- Only on MacOS or Linux
    opts = {},
    keys = {
      {
        -- Toggle copilotchat, the default is <leader>aa
        "<M-o>",
        function()
          return require("CopilotChat").toggle()
        end,
        desc = "Toggle (CopilotChat)",
        mode = { "n", "i", "v" },
      },
    }
  },
}
