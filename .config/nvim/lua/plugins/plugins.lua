return {
  -- color theme
  {
    "weiting1991/rose-pine.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      require "config.rose-pine"
      vim.cmd.colorscheme "rose-pine"
    end,
  },
  {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000,
    config = function()
      --require "config.catppuccin"
      -- vim.cmd.colorscheme "catppuccin"
    end,
  },
  -- treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    lazy = false,
    event = "VeryLazy",
    build = ":TSUpdate",
    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects",
    },
    config = function()
      require "config.treesitter"
    end,
  },
  -- syntax
  {
    "charlespascoe/vim-go-syntax",
    event = "BufEnter",
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
      },
      { "nvim-telescope/telescope-file-browser.nvim" },
      { "nvim-telescope/telescope-ui-select.nvim" },
      { "nvim-tree/nvim-web-devicons" },
    },
    config = function()
      require "config.telescope"
    end,
  },
  -- tree
  {
    "nvim-tree/nvim-tree.lua",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require "config.nvim-tree"
    end,
  },

  -- NOTE: Editer
  ---------------------------------------------------------------------------------------------------
  {
    "tpope/vim-sleuth",
    event = "BufEnter",
  },
  -- https://github.com/folke/todo-comments.nvim
  {
    "folke/todo-comments.nvim",
    event = "BufEnter",
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = {},
  },
  -- Comment
  -- https://github.com/numToStr/Comment.nvim
  {
    "numToStr/Comment.nvim",
    event = "BufEnter",
    opts = {
      -- add any options here
    },
  },
  -- Annotation
  {
    "danymat/neogen",
    event = "VeryLazy",
    dependencies = "nvim-treesitter/nvim-treesitter",
    config = function()
      require("neogen").setup { snippet_engine = "luasnip" }
      -- ref: https://github.com/danymat/neogen?tab=readme-ov-file#configuration
    end,
  },
  {
    "karb94/neoscroll.nvim",
    config = function()
      require("neoscroll").setup()
    end,
  },
  -- Copilot
  {
    "zbirenbaum/copilot.lua",
    cmd = "Copilot",
    event = "InsertEnter",
    config = function()
      require "config.copilot"
    end,
  },
  -- makdown preview
  {
    "iamcco/markdown-preview.nvim",
    cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
    event = "BufEnter",
    ft = { "markdown" },
    build = function()
      vim.fn["mkdp#util#install"]()
    end,
  },
  {
    -- https://github.com/epwalsh/obsidian.nvim
    enabled = false,
    "epwalsh/obsidian.nvim",
    version = "*", -- recommended, use latest release instead of latest commit
    lazy = true,
    ft = "markdown",
    config = function()
      require "config.obsidian"
    end,
  },
  -- harpoon
  {
    "ThePrimeagen/harpoon",
    event = "BufEnter",
    branch = "harpoon2",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      require "config.harpoon"
    end,
  },
  {
    "romgrk/barbar.nvim",
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
  -- notest
  -- ministarter
  -- gen.nvim
  --
  -------------------------------------------------------------------------------------------------
  --NOTE: UI
  -- transpant background
  {
    "xiyaowong/transparent.nvim",
    lazy = false,
    config = function()
      require "config.transparent"
    end,
  },
  -- statusline
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    event = "VimEnter",
    config = function()
      require "config.lualine"
    end,
  },
  -- colorizer
  {
    -- https://github.com/NvChad/nvim-colorizer.lua
    "NvChad/nvim-colorizer.lua",
    event = "BufEnter",
    config = function()
      require "config.colorizer"
    end,
  },
  -- NOTE: Useful website for icon.
  -- https://patorjk.com/software/taag/#p=display&f=Small&t=LE%60T%20WORK
  {
    "goolord/alpha-nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      local alpha = require "alpha"
      local dashboard = require "alpha.themes.startify"
      dashboard.section.header.val = {
        [[                                                         ]],
        [[                                                         ]],
        [[                                                         ]],
        [[                                                         ]],
        [[        _    ___ _ _____  __      _____  ___ _  _        ]],
        [[       | |  | __( )_   _| \ \    / / _ \| _ \ |/ /       ]],
        [[       | |__| _| \| | |    \ \/\/ / (_) |   / ' <        ]],
        [[       |____|___|   |_|     \_/\_/ \___/|_|_\_|\_\       ]],
        [[                                                         ]],
        [[                                                         ]],
        [[                                                         ]],
        [[                                                         ]],
      }
      alpha.setup(dashboard.opts)
    end,
  },
  --indentscope
  {
    "echasnovski/mini.indentscope",
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      require "config.indentscope"
    end,
  },
  {
    "folke/trouble.nvim",
    event = "BufEnter",
    config = function()
      require "config.trouble"
    end,
  },
}
