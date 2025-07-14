return {
  -- [[ Base UI ]]
  { "nvim-lua/plenary.nvim" },

  -- Statusline
  {
    "WeiTing1991/staline.nvim",
    priority = 1000,
    lazy = false,
    config = function()
      require "plugin.configs.staline"
    end,
  },

  --[[ CORE ]]
  -- treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    verson = false,
    event = "BufReadPre",
    -- load treesitter early when opening a file from the cmdline
    lazy = vim.fn.argc(-1) == 0,
    main = "nvim-treesitter.configs", -- Sets main module to use for opts
    build = ":TSUpdate",
    cmd = { "TSUpdateSync", "TSUpdate", "TSInstall" },
    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects",
      "nvim-treesitter/nvim-treesitter-context",
    },
    -- -@param opts TSConfig
    config = function()
      require "plugin.configs.treesitter"
    end,
  },
  -- fzf/telescope
  {
    "ibhagwan/fzf-lua",
    lazy = true,
    cmd = "FzfLua",
    event = "VeryLazy",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    opts = {},
    config = function()
      require "plugin.configs.fzf"
    end,
  },
  -- file tree
  {
    "stevearc/oil.nvim",
    lazy = false,
    event = "VeryLazy",
    ---@module 'oil'
    ---@type oil.SetupOpts
    opts = {},
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
