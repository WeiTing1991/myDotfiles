return {
  -- indention
  {
    "tpope/vim-sleuth",
    lazy = true,
    event = "BufRead",
  },

  -- Surround
  -- NOTE: cs'" ; ysiw)
  {
    "kylechui/nvim-surround",
    lazy = true,
    event = "VeryLazy",
    config = function()
      require("nvim-surround").setup({})
    end,
  },

  -- Better sellect with a and i
  {
    "echasnovski/mini.ai",
    lazy = true,
    event = "InsertEnter",
    version = "*",
    config = function()
      require("mini.ai").setup()
    end,
  },

  -- Autoclosing braces
  {
    "windwp/nvim-autopairs",
    lazy = true,
    event = "InsertEnter",
    config = function()
      local npairs = require("nvim-autopairs")
      npairs.setup()
      -- local rule = require("nvim-autopairs.rule")
      -- local conds = require("nvim-autopairs.conds")
      -- Autoclosing angle-brackets.
      -- npairs.add_rule(rule("<", ">", {
      --     -- Avoid conflicts with nvim-ts-autotag.
      --     "-html",
      --     "-javascriptreact",
      --     "-typescriptreact",
      --   })
      --   :with_pair(function(opts)
      --     -- Don't pair angle brackets when inside parentheses
      --     local line = opts.line
      --     local pos = opts.col
      --
      --     -- Check if we're inside parentheses by counting opening/closing parens
      --     local open_count = 0
      --     local close_count = 0
      --     for i = 1, pos do
      --       if line:sub(i, i) == "(" then
      --         open_count = open_count + 1
      --       elseif line:sub(i, i) == ")" then
      --         close_count = close_count + 1
      --       end
      --     end
      --
      --     -- If we have more opening than closing parens, we're inside parentheses
      --     if open_count > close_count then
      --       return false
      --     end
      --
      --     -- Original condition for angle brackets
      --     return conds.before_regex("%a+:?:?$", 3)(opts)
      --   end)
      --   :with_move(function(opts)
      --     return opts.char == ">"
      --   end))
    end,
  },
  -- Better Comment
  {
    "numToStr/Comment.nvim",
    lazy = true,
    event = "VeryLazy",
    opts = {},
  },
}
