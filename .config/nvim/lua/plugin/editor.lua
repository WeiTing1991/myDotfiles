return {
  {
    -- https://github.com/folke/todo-comments.nvim
    "folke/todo-comments.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    event = "VeryLazy",
    opts = {},
  },
  { -- comment
    "numToStr/Comment.nvim",
    event = "VeryLazy",
    opts = {
      -- add any options here
    },
  },
  {
    "windwp/nvim-autopairs",
    --enabled = false,
    event = "VeryLazy",
    -- Optional dependency
    dependencies = { "hrsh7th/nvim-cmp" },
    config = function()
      require("nvim-autopairs").setup({})
      -- If you want to automatically add `(` after selecting a function or method
      local cmp_autopairs = require("nvim-autopairs.completion.cmp")
      local cmp = require("cmp")
      cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done())
    end,
  },
  {
    --Annotation
    "danymat/neogen",
    event = "VeryLazy",
    dependencies = "nvim-treesitter/nvim-treesitter",
    config = function()
      require("neogen").setup({})
      -- ref: https://github.com/danymat/neogen?tab=readme-ov-file#configuration
      --languages = {
      --	["python.google_docstrings"] = require("neogen.configurations.python"),
      --}
    end,
  },
  {
    -- https://github.com/NvChad/nvim-colorizer.lua
    "NvChad/nvim-colorizer.lua",
    event = "VeryLazy",
    config = function()
      require("colorizer").setup()
    end,
  },
}
