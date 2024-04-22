return {
  {
    "theprimeagen/vim-be-good",
    event = "VeryLazy",
    dependencies = {
      "nvim-lua/plenary.nvim",
    },

    config = function() end,
  },
  {
    "stevearc/oil.nvim",
    opts = {
      view_options = {
        show_hidden = true,
      },
    },
    -- Optional dependencies
    dependencies = { "nvim-tree/nvim-web-devicons" },
  },
}
