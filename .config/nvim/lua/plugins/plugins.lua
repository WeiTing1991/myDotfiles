return {
  -- --------------------------- Base UI ---------------------------------

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

  -- --------------------------- Core   ---------------------------------
  -- fzf/telescope

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
      { "nvim-telescope/telescope-frecency.nvim", version = "*" },
      { "nvim-telescope/telescope-ui-select.nvim" },
      --{ "nvim-tree/nvim-web-devicons" },
    },
    config = function()
      require "config.telescope"
    end,
  },

  -- treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    event = "VimEnter",
    build = ":TSUpdate",
    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects",
    },
    -- -@param opts TSConfig
    config = function()
      require "config.treesitter"
    end,
  },

  -- file tree
  {
    "stevearc/oil.nvim",
    event = "VimEnter",
    ---@module 'oil'
    ---@type oil.SetupOpts
    opts = {},
    -- Optional dependencies
    dependencies = { { "echasnovski/mini.icons", opts = {} } },
    config = function()
      require "config.oil"
    end,
  },

  -- ---------------------------- EDITOR ------------------------------

  -- todo highlight
  {
    "folke/todo-comments.nvim",
    event = "BufRead",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      require "config.todo"
    end,
  },
  -- indentscope
  {
    "tpope/vim-sleuth",
    event = "VimEnter",
  },
  {
    "echasnovski/mini.indentscope",
    version = false,
    event = "VimEnter",
    config = function()
      require "config.indentscope"
    end,
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    event = "VimEnter",
    main = "ibl",
    ---@module "ibl"
    ---@type ibl.config
    opts = {},
    config = function()
      require("ibl").setup {
        exclude = {
          filetypes = { "markdown" },
          buftypes = { "terminal" },
        },
        indent = { char = "▏" },
        scope = { enabled = false },
      }
    end,
  },
  {
    "lukas-reineke/virt-column.nvim",
    event = "BufEnter",
    opts = {
      char = { "┆" },
      virtcolumn = "120",
      highlight = { "NonText" },
    },
  },

  -- ---------------------------- TOOL ------------------------------
  --
  -- show keymaps
  -- NOTE: https://github.com/echasnovski/mini.nvim/blob/main/readmes/mini-clue.md
  {
    "echasnovski/mini.clue",
    version = "*",
    event = "VeryLazy",
    config = function()
      require "config.custom-keymaps"
    end,
  },

  -- which-key
  -- {
  --   "folke/which-key.nvim",
  --   enabled = false,
  --   event = "VeryLazy",
  --   opts = {
  --   },
  --   keys = {
  --     {
  --       "<leader>?",
  --       function()
  --         require("which-key").show { global = false }
  --       end,
  --       desc = "Buffer Local Keymaps (which-key)",
  --     },
  --   },
  -- },

  {
    "folke/zen-mode.nvim",
    event = "BufEnter",
    opts = {
    },
  },

}
