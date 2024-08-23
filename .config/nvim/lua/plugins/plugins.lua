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
          return vim.fn.executable 'make' == 1
        end,
      },
    },
    config = function()
        require "config.telescope"
    end,
  },
  -- treesitter
 {
    'nvim-treesitter/nvim-treesitter',
    build = ':TSUpdate',
    config = function()
      require "config.treesitter"
    end,
  },
}
