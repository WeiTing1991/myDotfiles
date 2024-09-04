return {
  -- color theme
  {
    "weiting1991/rose-pine.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      require "config.rose-pine"
      vim.cmd.colorscheme "rose-pine"
    end,
  },
  {
    "tpope/vim-sleuth",
  },
  -- telescope
  {
    "nvim-telescope/telescope.nvim",
    event = "VimEnter",
    branch = "0.1.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
      {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = "make",
        cond = function()
          return vim.fn.executable "make" == 1
        end,
      },
      { "nvim-tree/nvim-web-devicons" },
    },
    config = function()
      require "config.telescope"
    end,
  },
  -- treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    config = function()
      require "config.treesitter"
    end,
  },
  -- gitool
  {
    "lewis6991/gitsigns.nvim",
    config = function()
      require("gitsigns").setup {
        signs = {
          add = { text = "+" },
          change = { text = "~" },
          delete = { text = "_" },
          topdelete = { text = "‾" },
          changedelete = { text = "~" },
        },
        current_line_blame = true, -- Toggle with `:Gitsigns toggle_current_line_blame`
        current_line_blame_formatter = "<author>, <author_time:%Y-%m-%d> - <summary>",
      }
      -- NOTE: https://github.com/lewis6991/gitsigns.nvim
      -- keymaps
      vim.keymap.set("n", "<leader>gh", ":Gitsign preview_hunk<CR>", { desc = "Preview hunk" })
      vim.keymap.set(
        "n",
        "<leader>gb",
        ":Gitsign toggle_current_line_blame<CR>",
        { desc = "toggle current line blame" }
      )
    end,
  },
  -- tree
  {
    "nvim-tree/nvim-tree.lua",
    event = "VimEnter",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require "config.nvim-tree"
    end,
  },
  -- editors
  {
    "romgrk/barbar.nvim",
    event = "VimEnter",
    init = function()
      vim.g.barbar_auto_setup = false
    end,
    version = "^1.0.0", -- optional: only update when a new 1.x version is released
    opts = {
      animation = false,
      highlights_alternative = true,
      button = " ",
      gitsigns = {
        added = { enabled = false, icon = "+" },
        changed = { enabled = false, icon = "~" },
        deleted = { enabled = false, icon = "-" },
      },
    },
  },

  {
    "zbirenbaum/copilot.lua",
    cmd = "Copilot",
    event = "InsertEnter",
    config = function()
      require "config.copilot"
    end,
  },
}
