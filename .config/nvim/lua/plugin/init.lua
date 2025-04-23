return {
  -- [[ Base UI ]]
  { "nvim-lua/plenary.nvim" },

  -- Statusline
  {
    "WeiTing1991/staline.nvim",
    lazy = true,
    event = "VimEnter",
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
      require("treesitter-context").setup {
        enable = true,
        multiwindow = true,
        max_lines = 0,
        min_window_height = 0,
        line_numbers = true,
        multiline_threshold = 20,
        trim_scope = "outer",
        mode = "cursor",
        oeparator = nil,
        zindex = 20,
        on_attach = nil,
      }
    end,
    vim.api.nvim_set_hl(0, "TreesitterContext", { underline = true, sp = "Grey", bold = true }),
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
    lazy = true,
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
