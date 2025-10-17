return {
  -- [[ Base UI ]]
  { "nvim-lua/plenary.nvim" },
  { "nvim-tree/nvim-web-devicons", lazy = true },

  {
    "nvchad/ui",
    config = function()
      require("nvchad")
    end,
  },

  {
    "nvchad/base46",
    lazy = true,
    dependencies = { "nvchad/volt" },
    build = function()
      require("base46").load_all_highlights()
    end,
  },

  --[[ CORE ]]
  -- treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    main = "nvim-treesitter.configs",
    lazy = false,
    version = false,
    build = ":TSUpdate",
    cmd = { "TSUpdateSync", "TSUpdate", "TSInstall" },
    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects",
      "nvim-treesitter/nvim-treesitter-context",
    },
    config = function()
      require("plugins.configs.treesitter")
    end,
  },

  -- telescope
  {
    "nvim-telescope/telescope.nvim",
    event = "VimEnter",
    dependencies = {
      "nvim-lua/plenary.nvim",
      {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = "make",
        cond = function()
          return vim.fn.executable("make") == 1
        end,
      },
      { "nvim-tree/nvim-web-devicons" },
      { "nvim-telescope/telescope-ui-select.nvim" },
      { "nvim-tree/nvim-web-devicons", enabled = vim.g.have_nerd_font },
    },
    config = function()
      require("plugins.configs.telescope")
    end,
  },

  -- file tree
  {
    "stevearc/oil.nvim",
    opts = {},
    event = "VeryLazy",
    lazy = true,
    config = function()
      require("plugins.configs.oil")
    end,
  },
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "MunifTanjim/nui.nvim",
      "nvim-tree/nvim-web-devicons",
      "antosha417/nvim-lsp-file-operations",
    },
    lazy = false, -- neo-tree will lazily load itself
    config = function()
      require("plugins.configs.neotree")
      require("lsp-file-operations").setup()
    end,
  },
  -- emacs-like keymaps
  {
    "echasnovski/mini.clue",
    version = "*",
    lazy = true,
    event = "VeryLazy",
    config = function()
      require("plugins.configs.miniclue")
    end,
  },
}
