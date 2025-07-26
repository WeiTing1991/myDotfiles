return {
  -- [[ Base UI ]]
  { "nvim-lua/plenary.nvim" },

  --[[ CORE ]]
  -- treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    main = 'nvim-treesitter.configs',
    event = { "VeryLazy" },
    lazy = vim.fn.argc(-1) == 0,
    build = ":TSUpdate",
    cmd = { "TSUpdateSync", "TSUpdate", "TSInstall" },
    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects",
      "nvim-treesitter/nvim-treesitter-context",
    },
    config = function()
      require "plugin.configs.treesitter"
    end,
  },

  -- fzf/telescope
  {
    'nvim-telescope/telescope.nvim',
    event = 'VimEnter',
    dependencies = {
      {
        'nvim-telescope/telescope-fzf-native.nvim',
        build = 'make',
        cond = function()
          return vim.fn.executable 'make' == 1
        end,
      },
      { 'nvim-telescope/telescope-ui-select.nvim' },
      { 'nvim-tree/nvim-web-devicons', enabled = vim.g.have_nerd_font },
    },
    config = function()
      require "plugin.configs.telescope"
    end,
  },

  -- file tree
  {
    "stevearc/oil.nvim",
    ---@module 'oil'
    ---@type oil.SetupOpts
    opts = {},
    event = "VeryLazy",
    lazy = false,
    config = function()
      require "plugin.configs.oil"
    end,
  },

  -- emacs-like keymaps
  {
    "echasnovski/mini.clue",
    version = "*",
    lazy = true,
    event = "VeryLazy",
    config = function()
      require "plugin.configs.miniclue"
    end,
  },
}
