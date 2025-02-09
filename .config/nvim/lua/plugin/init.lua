return {
  ---------------------------------------- Base UI ----------------------------------------------------
  { "nvim-lua/plenary.nvim" },
  { "nvim-tree/nvim-web-devicons", lazy = true },
  {
    "nvchad/ui",
    -- lazy = true,
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

  --------------------------------------------- CORE  -------------------------------------------------------
  -- fzf/telescope
  --
  {
    "ibhagwan/fzf-lua",
    -- lazy = true,
    dependencies = { "nvim-tree/nvim-web-devicons" },
    opts = {},
    config = function()
      require "configs.fzf"
    end
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

  -- {
  --   "nvim-telescope/telescope.nvim",
  --   lazy = true,
  --   cmd = "Telescope",
  --   event = "VimEnter",
  --   branch = "0.1.x",
  --   dependencies = {
  --     {
  --       "nvim-telescope/telescope-fzf-native.nvim",
  --       build = "make",
  --       cond = function()
  --         return vim.fn.executable "make" == 1
  --       end,
  --     },
  --     { "nvim-telescope/telescope-frecency.nvim", version = "*" },
  --     { "nvim-telescope/telescope-ui-select.nvim" },
  --   },
  --   config = function()
  --     require "configs.telescope"
  --   end,
  -- },

  -- treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    version = false,
    lazy = true,
    -- load treesitter early when opening a file from the cmdline
    lazy = vim.fn.argc(-1) == 0,
    event = "VeryLazy",
    build = ":TSUpdate",
    cmd = { "TSUpdateSync", "TSUpdate", "TSInstall" },
    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects",
    },
    keys = {
      { "<c-space>", desc = "Increment Selection" },
      { "<bs>", desc = "Decrement Selection", mode = "x" },
    },
    -- -@param opts TSConfig
    config = function()
      require "configs.treesitter"
    end,
  },

  -- {
  --   "nvim-tree/nvim-tree.lua",
  --   version = "*",
  --   lazy = true,
  --   event = "VimEnter",
  --   config = function()
  --     require "configs.nvimtree"
  --   end,
  -- },

  -- emacs-like keymaps
  -- NOTE: https://github.com/echasnovski/mini.nvim/blob/main/readmes/mini-clue.md
  {
    "echasnovski/mini.clue",
    version = "*",
    lazy = true,
    event = "VeryLazy",
  },
}
