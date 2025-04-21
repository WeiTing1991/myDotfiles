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

      require("mini.indentscope").setup {
        draw = {
          delay = 0,
          animation = function()
            return 0
          end,
          priority = 2,
        },
        symbol = "▏",
        options = { border = "top", try_as_border = true },
      }
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
      require("nvim-surround").setup {}
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
      local npairs = require "nvim-autopairs"
      local rule = require "nvim-autopairs.rule"
      local conds = require "nvim-autopairs.conds"

      npairs.setup()

      -- Autoclosing angle-brackets.
      npairs.add_rule(rule("<", ">", {
          -- Avoid conflicts with nvim-ts-autotag.
          "-html",
          "-javascriptreact",
          "-typescriptreact",
        })
        :with_pair(conds.before_regex("%a+:?:?$", 3))
        :with_move(function(opts)
          return opts.char == ">"
        end))
    end,
  },
  {
    "Wansmer/treesj",
    lazy = true,
    event = "InsertEnter",
    dependencies = "nvim-treesitter",
    keys = {
      { "<leader>tj", "<cmd>TSJToggle<cr>", desc = "Join/split code block" },
    },
    opts = { use_default_keymaps = false },
  },

  -- Better Comment
  {
    "numToStr/Comment.nvim",
    lazy = true,
    event = "VeryLazy",
    opts = {},
    -- config = function()
    --   require("Comment").setup {
    --     pre_hook = require("ts_context_commentstring.integrations.comment_nvim").create_pre_hook(),
    --   }
    -- end,
  },

  -- Not working
  {
    "mg979/vim-visual-multi",
    lazy = true,
    enabled = false,
    event = "InsertEnter",
    config = function()
      vim.g.VM_default_mappings = 0
      vim.g.VM_maps = {
        ["Find Under"] = "<M-d>",
        ["Find Subword Under"] = "<M-d>",
      }
      print(vim.g.VM_maps["Find Under"])

      -- Tried these as well but they do not work.
      -- vim.g.VM_maps['Find Subword Under'] = "<C-x>"
      -- vim.g.VM_maps["Select Cursor Down"] = '<M-u>'
      -- vim.g.VM_maps["Select Cursor Up"]   = '<M-d>'
    end,
  },
}
