return {
  ---------------------------------------- Base UI ----------------------------------------------------
  { "nvim-lua/plenary.nvim" },
  {
    "nvchad/ui",
    config = function()
      require "nvchad"
    end,
  },
  {
    "nvchad/base46",
    lazy = true,
    build = function()
      require("base46").load_all_highlights()
    end,
  },

  -- statusline
  {
    "WeiTing1991/staline.nvim",
    lazy = false,
    config = function()
      require "config.staline"
    end,
  },

  --------------------------------------- CORE  -------------------------------------------------------
  -- fzf/telescope
  -- {
  --   "nvim-telescope/telescope.nvim",
  --   event = "VimEnter",
  --   branch = "0.1.x",
  --   dependencies = {
  --     "nvim-lua/plenary.nvim",
  --     {
  --       "nvim-telescope/telescope-fzf-native.nvim",
  --       build = "make",
  --       cond = function()
  --         return vim.fn.executable "make" == 1
  --       end,
  --     },
  --     { "nvim-telescope/telescope-frecency.nvim", version = "*" },
  --     { "nvim-telescope/telescope-ui-select.nvim" },
  --     --{ "nvim-tree/nvim-web-devicons" },
  --   },
  --   config = function()
  --     require "config.telescope"
  --   end,
  -- },
  --
  -- -- treesitter
  -- {
  --   "nvim-treesitter/nvim-treesitter",
  --   event = "VimEnter",
  --   build = ":TSUpdate",
  --   dependencies = {
  --     "nvim-treesitter/nvim-treesitter-textobjects",
  --   },
  --   -- -@param opts TSConfig
  --   config = function()
  --     require "config.treesitter"
  --   end,
  -- },
  --
  -- -- file tree
  -- {
  --   "stevearc/oil.nvim",
  --   event = "VimEnter",
  --   ---@module 'oil'
  --   ---@type oil.SetupOpts
  --   opts = {},
  --   -- Optional dependencies
  --   dependencies = { { "echasnovski/mini.icons", opts = {} } },
  --   config = function()
  --     require "config.oil"
  --   end,
  -- },
  --
  -- -- -------------------------------- EDITOR  -----------------------------------------------------
  -- -- todo highlight
  -- {
  --   "folke/todo-comments.nvim",
  --   event = "BufRead",
  --   dependencies = { "nvim-lua/plenary.nvim" },
  --   config = function()
  --     require "config.todo"
  --   end,
  -- },
  --
  -- {
  --   "tpope/vim-sleuth",
  --   event = "BufRead",
  -- },
  --
  -- -- indentscope
  -- {
  --   "echasnovski/mini.indentscope",
  --   version = false,
  --   event = "VimEnter",
  --   config = function()
  --     require "config.indentscope"
  --   end,
  -- },
  -- {
  --   "lukas-reineke/indent-blankline.nvim",
  --   event = "VimEnter",
  --   main = "ibl",
  --   ---@module "ibl"
  --   ---@type ibl.config
  --   opts = {},
  --   config = function()
  --     require("ibl").setup {
  --       exclude = {
  --         filetypes = { "markdown" },
  --         buftypes = { "terminal" },
  --       },
  --       indent = { char = "▏" },
  --       scope = { enabled = false },
  --     }
  --   end,
  -- },
  --
  -- -- column line
  -- {
  --   "lukas-reineke/virt-column.nvim",
  --   event = "BufEnter",
  --   opts = {
  --     char = { "┆" },
  --     virtcolumn = "105",
  --     highlight = { "NonText" },
  --     exclude = { filetypes = { "oil", "markdown" } },
  --   },
  -- },
  --
  -- -- Better Comment
  -- {
  --   "numToStr/Comment.nvim",
  --   event = "InsertEnter",
  --   opts = {},
  -- },
  --
  -- -- Autopair
  -- {
  --   "echasnovski/mini.pairs",
  --   event = "InsertEnter",
  --   version = "*",
  --   config = function()
  --     require("mini.pairs").setup()
  --   end,
  -- },
  --
  -- -- Better sellect with a i
  -- -- NOTE: https://github.com/echasnovski/mini.nvim/blob/main/readmes/mini-ai.md
  -- {
  --   "echasnovski/mini.ai",
  --   event = "InsertEnter",
  --   version = "*",
  --   config = function()
  --     require("mini.ai").setup()
  --   end,
  -- },
  --
  -- -- -------------------------------- TOOL  -------------------------------------------------------
  -- -- Zen mode
  -- {
  --   "folke/zen-mode.nvim",
  --   event = "BufEnter",
  --   opts = {},
  -- },
  --
  -- -- NOTE: https://github.com/folke/trouble.nvim?tab=readme-ov-file
  -- {
  --   "folke/trouble.nvim",
  --   opts = {},
  --   cmd = "Trouble",
  -- },
  --
  -- -- git tools
  -- {
  --   "lewis6991/gitsigns.nvim",
  --   event = "VimEnter",
  --   config = function()
  --     require("gitsigns").setup {
  --       signs = {
  --         -- add = { text = "+" },
  --         change = { text = "~" },
  --         -- delete = { text = "_" },
  --         -- topdelete = { text = "‾" },
  --         changedelete = { text = "│" },
  --         -- untracked    = { text = '┆' },
  --       },
  --       current_line_blame = false, -- Toggle with `:Gitsigns toggle_current_line_blame`
  --       current_line_blame_formatter = "<author>, <author_time:%Y-%m-%d>-<summary>",
  --     }
  --   end,
  -- },
  --
  -- -- ai tools
  -- {
  --   "zbirenbaum/copilot.lua",
  --   cmd = "Copilot",
  --   event = "InsertEnter",
  --   config = function()
  --     require "config.copilot"
  --   end,
  -- },
  --
  -- --keymaps
  -- -- NOTE: https://github.com/echasnovski/mini.nvim/blob/main/readmes/mini-clue.md
  -- {
  --   "echasnovski/mini.clue",
  --   version = false,
  --   event = "VeryLazy",
  --   config = function()
  --     require "config.custom-keymaps"
  --   end,
  -- },
}
