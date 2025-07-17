return {
  -- indention
  {
    "tpope/vim-sleuth",
    lazy = true,
    event = "BufRead",
  },

  --blankline
  {
    "echasnovski/mini.indentscope",
    version = false,
    lazy = true,
    event = "BufRead",
    config = function()
      vim.api.nvim_set_hl(0, "MiniIndentscopeSymbol", { fg = "#e5e9f0", default = true })
      vim.api.nvim_set_hl(0, "MiniIndentscopeSymbolOff", { default = true, link = "MiniIndentscopeSymbol" })

      require("mini.indentscope").setup({
        draw = {
          delay = 0,
          animation = function()
            return 0
          end,
          priority = 2,
        },
        symbol = "▏",
        options = { border = "top", try_as_border = true },
      })
      -- Disable for certain filetypes
      vim.api.nvim_create_autocmd("FileType", {
        pattern = {
          "lspinfo",
          "checkhealth",
          "help",
          "lazy",
          "mason",
          "markdown",
          "nvimtree",
          "oil",
          "fzf",
        },
        callback = function()
          vim.b.miniindentscope_disable = true
        end,
      })
    end,
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    lazy = true,
    event = "BufRead",
    main = "ibl",
    opts = {
      indent = { char = "▏" },
      scope = {
        show_start = false,
        show_end = false,
      },
      exclude = {
        filetypes = { "markdown" },
        buftypes = { "terminal", "oil" },
      },
    },
  },

  -- column line
  {
    "lukas-reineke/virt-column.nvim",
    lazy = true,
    event = "BufRead",
    opts = {
      char = { "┆" },
      virtcolumn = "120",
      highlight = { "NonText" },
      exclude = { filetypes = { "oil", "markdown", "fzf" } },
    },
  },

  -- surround select
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
      local rule = require("nvim-autopairs.rule")
      local conds = require("nvim-autopairs.conds")

      npairs.setup()

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
