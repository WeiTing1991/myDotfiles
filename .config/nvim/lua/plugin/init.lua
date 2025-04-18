return {
  -- [[ Base UI ]]
  { "nvim-lua/plenary.nvim" },
  -- { "nvim-tree/nvim-web-devicons", lazy = true },

  -- Statusline
  -- {
  --   "WeiTing1991/staline.nvim",
  --   lazy = false,
  --   config = function()
  --     require "configs.staline"
  --   end,
  -- },

  --[[ CORE ]]
  -- treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    event = "BufReadPre",
    -- load treesitter early when opening a file from the cmdline
    lazy = vim.fn.argc(-1) == 0,
    main = 'nvim-treesitter.configs', -- Sets main module to use for opts
    build = ":TSUpdate",
    cmd = { "TSUpdateSync", "TSUpdate", "TSInstall" },
    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects",
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
    event = "VeryLazy",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    opts = {},
    config = function()
      require "plugin.configs.fzf"
    end
  },
  -- file tree
  {
    "stevearc/oil.nvim",
    lazy = true,
    event = "VeryLazy",
    ---@module 'oil'
    ---@type oil.SetupOpts
    opts = {},
    config = function()
      require "plugin.configs.oil"
    end,
  },
}
