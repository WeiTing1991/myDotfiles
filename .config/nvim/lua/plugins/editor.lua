return {
  -- Auto detect indentation
  {
    "tpope/vim-sleuth",
    event = "BufReadPost",
  },

  -- Surround (cs'" ; ysiw))
  {
    "kylechui/nvim-surround",
    event = "BufReadPost",
    config = function()
      require("nvim-surround").setup({})
    end,
  },

  -- Better select with a and i
  {
    "echasnovski/mini.ai",
    event = "BufReadPost",
    version = "*",
    config = function()
      require("mini.ai").setup()
    end,
  },

  -- Autoclosing braces
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    config = function()
      require("nvim-autopairs").setup()
    end,
  },

  -- Comment
  {
    "numToStr/Comment.nvim",
    event = "BufReadPost",
    opts = {},
  },
}
