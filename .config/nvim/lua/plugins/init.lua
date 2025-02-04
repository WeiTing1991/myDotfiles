return {
  ---------------------------------------- Base UI ----------------------------------------------------
  { "nvim-lua/plenary.nvim" },
  { "nvim-tree/nvim-web-devicons", lazy = true },
  {
    "nvchad/ui",
    config = function()
      require "nvchad"
    end
  },
  {
    "nvchad/base46",
    lazy = true,
    build = function()
      require("base46").load_all_highlights()
    end,
  },
  --theme switcher
  { "nvchad/volt" },

  -- statusline
  {
    "WeiTing1991/staline.nvim",
    lazy = false,
    config = function()
      require "configs.staline"
    end,
  },

  --------------------------------------- CORE  -------------------------------------------------------
  -- fzf/telescope
  {
    "nvim-telescope/telescope.nvim",
    lazy = true,
    cmd = "Telescope",
    event = "VimEnter",
    branch = "0.1.x",
    dependencies = {
      {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = "make",
        cond = function()
          return vim.fn.executable "make" == 1
        end,
      },
      { "nvim-telescope/telescope-frecency.nvim", version = "*" },
      { "nvim-telescope/telescope-ui-select.nvim" },
    },
    config = function()
      require "configs.telescope"
    end,
  },

  -- treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    lazy = true,
    event = "VimEnter",
    build = ":TSUpdate",
    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects",
    },
    -- -@param opts TSConfig
    config = function()
      require "configs.treesitter"
    end,
  },

  -- file tree
  {
    "stevearc/oil.nvim",
    lazy = true,
    event = "VimEnter",
    ---@module 'oil'
    ---@type oil.SetupOpts
    opts = {},
    config = function()
      require "configs.oil"
    end,
  },
  {
    "nvim-tree/nvim-tree.lua",
    version = "*",
    lazy = true,
    event = "VimEnter",
    config = function()
      require "configs.nvimtree"
    end,
  },

  -- emacs-like keymaps
  -- NOTE: https://github.com/echasnovski/mini.nvim/blob/main/readmes/mini-clue.md
  {
    "echasnovski/mini.clue",
    lazy = true,
    version = "*",
    event = "VeryLazy",
    -- config = function()
    --   require "mapping"
    -- end,
  },
}
