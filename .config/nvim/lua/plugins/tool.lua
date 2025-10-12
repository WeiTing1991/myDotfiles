return {
  -- tmux navigator
  {
    "christoomey/vim-tmux-navigator",
    lazy = true,
    event = "BufEnter",
    enabled = false,
    cmd = {
      "TmuxNavigateLeft",
      "TmuxNavigateDown",
      "TmuxNavigateUp",
      "TmuxNavigateRight",
      "TmuxNavigatePrevious",
    },
    keys = {
      { "<c-h>", "<cmd><C-U>TmuxNavigateLeft<cr>" },
      { "<c-j>", "<cmd><C-U>TmuxNavigateDown<cr>" },
      { "<c-k>", "<cmd><C-U>TmuxNavigateUp<cr>" },
      { "<c-l>", "<cmd><C-U>TmuxNavigateRight<cr>" },
      { "<c-\\>", "<cmd><C-U>TmuxNavigatePrevious<cr>" },
    },
  },

  -- diagnostics
  {
    "folke/trouble.nvim",
    lazy = true,
    event = "VeryLazy",
    cmd = "Trouble",
    opts = {
      focus = true,
      auto_preview = false, -- Disable auto-preview
      preview = {
        type = "float",
        relative = "editor",
        size = { width = 0.8, height = 0.3 }, -- Smaller preview
        position = { 0.5, 0.8 },
        border = "single",
      },
    },
  },
}
