return {

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
  -- colortheme
  {
    "weiting1991/rose-pine.nvim",
    -- lazy = false,
    priority = 1000,
    config = function()
      require "config.rose-pine"
      vim.cmd.colorscheme "rose-pine"
      vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
      vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
    end,
  },
  {
    "gbprod/nord.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      require "config.nord"
      -- vim.cmd.colorscheme "nord"
      -- vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
      -- vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
    end,
  },
  install = {
    colorscheme = { "nord" },
  },
  {
    "xiyaowong/transparent.nvim",
    -- lazy = false,
    config = function()
      require "config.transparent"
    end,
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
      { "nvim-telescope/telescope-ui-select.nvim" },
      { "nvim-telescope/telescope-frecency.nvim" },
    },
    config = function()
      require "config.telescope"
    end,
  },

  -- treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects",
    },
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
          changedelete = { text = "│" },
        },
        current_line_blame = true, -- Toggle with `:Gitsigns toggle_current_line_blame`
        current_line_blame_formatter = "<author>, <author_time:%Y-%m-%d>-<summary>",
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

  -- quickfix
  {
    "folke/trouble.nvim",
    event = "BufEnter",
    opts = {},
    cmd = "Trouble",
    config = function()
      require "config.trouble"
    end,
  },

  -- file tree
  {
    "nvim-tree/nvim-tree.lua",
    event = "VimEnter",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require "config.nvim-tree"
    end,
  },
  {
    "stevearc/oil.nvim",
    ---@module 'oil'
    ---@type oil.SetupOpts
    opts = {},
    -- Optional dependencies
    dependencies = { { "echasnovski/mini.icons", opts = {} } },
    config = function()
      require "config.oil"
    end,
  },
  {
    "echasnovski/mini.files",
    version = false,
    config = function()
      require("mini.files").setup {
        windows = {
          max_number = math.huge,
          preview = true,
          width_focus = 30,
          width_nofocus = 10,
          width_preview = 100,
        },
      }
    end,
  },

  -- editors
  {
    "tpope/vim-sleuth",
  },
  {
    "romgrk/barbar.nvim",
    enabled = false,
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
    "echasnovski/mini.indentscope",
    version = false,
    config = function()
      require "config.indentscope"
    end,
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    ---@module "ibl"
    ---@type ibl.config
    opts = {},
    config = function()
      require("ibl").setup {
        indent = { char = "▏" },
        scope = { enabled = false },
      }
    end,
  },
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require "config.lualine"
    end,
  },
  {
    "numToStr/Comment.nvim",
    opts = {},
  },
  {
    "mbbill/undotree",
    event = "VeryLazy",
  },
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    config = true,
    -- use opts = {} for passing setup options
    -- this is equivalent to setup({}) function
  },
  {
    "echasnovski/mini.hipatterns",
    event = "BufReadPre",
    opts = {},
  },

  -- ui
  {
    "lukas-reineke/virt-column.nvim",
    opts = {
      char = { "┆" },
      -- virtcolumn = "120",
      highlight = { "NonText" },
    },
  },

  -- ai system
  {
    "zbirenbaum/copilot.lua",
    cmd = "Copilot",
    event = "InsertEnter",
    config = function()
      require "config.copilot"
    end,
  },

  -- auto session
  {
    "rmagatti/auto-session",
    lazy = false,

    ---enables autocomplete for opts
    ---@module "auto-session"
    ---@type AutoSession.Config
    config = function()
      require "config.auto-session"
    end,
  },
}
