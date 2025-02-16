return {
  ---------------------------------- EDITOR  ---------------------------------------------
  -- todo highlight
  {
    "folke/todo-comments.nvim",
    lazy = true,
    event = "BufRead",
    config = function()
      require "configs.todo"
    end,
  },

  -- indention
  {
    "tpope/vim-sleuth",
    lazy = true,
    event = "BufRead",
  },

  -- indentscope
  {
    "echasnovski/mini.indentscope",
    version = false,
    lazy = true,
    event = "BufRead",
    config = function()
      require "configs.indentscope"
    end,
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    lazy = true,
    event = "BufRead",
    main = "ibl",
    ---@module "ibl"
    ---@type ibl.config
    opts = {},
    config = function()
      require("ibl").setup {
        exclude = {
          filetypes = { "markdown" },
          buftypes = { "terminal", "oil" },
        },
        indent = { char = "▏" },
        scope = { enabled = false },
      }
    end,
  },
  -- column line
  {
    "lukas-reineke/virt-column.nvim",
    lazy = true,
    event = "BufRead",
    opts = {
      char = { "┆" },
      virtcolumn = "110",
      highlight = { "NonText" },
      exclude = { filetypes = { "oil", "markdown", "fzf" } },
    },
  },
  -- Better Comment
  {
    "numToStr/Comment.nvim",
    lazy = true,
    event = "VeryLazy",
    opts = {},
    config = function()
      require('Comment').setup {
        pre_hook = require('ts_context_commentstring.integrations.comment_nvim').create_pre_hook(),
      }
    end
  },
  -- Autopair
  {
    "windwp/nvim-autopairs",
    enabled = true,
    lazy = true,
    event = "InsertEnter",
    opts = {
      fast_wrap = {},
      disable_filetype = { "TelescopePrompt", "vim" },
    },
    config = function(_, opts)
      require("nvim-autopairs").setup(opts)
    end,
  },

  -- Better search
  --NOTE: https://github.com/MagicDuck/grug-far.nvim/blob/main/lua/grug-far/opts.lua
  {
    "MagicDuck/grug-far.nvim",
    opts = { headerMaxWidth = 80 },
    lazy = true,
    -- event = "BufRead",
    cmd = "GrugFar",
    keys = {
      {
        "<leader>/",
        function()
          local grug = require("grug-far")
          local ext = vim.bo.buftype == "" and vim.fn.expand("%:e")
          grug.open({
            transient = true,
            prefills = {
              filesFilter = ext and ext ~= "" and "*." .. ext or nil,
            },
          })
        end,
        mode = { "n", "v" },
        desc = "Search and Replace",
      },
    },
  },
  -- NOTE: https://github.com/kylechui/nvim-surround/blob/main/doc/nvim-surround.txt
  {
    "echasnovski/mini.surround",
    lazy = true,
    event = "InsertEnter",
    version = false,
    config = function()
      require("mini.surround").setup()
    end
  },
  -- Better sellect with a and i
  -- NOTE: https://github.com/echasnovski/mini.nvim/blob/main/readmes/mini-ai.md
  {
    "echasnovski/mini.ai",
    lazy = true,
    event = "InsertEnter",
    version = "*",
    config = function()
      require("mini.ai").setup()
    end,
  },

  {
    'echasnovski/mini.align',
    version = '*',
    lazy = true,
    event = 'InsertEnter',
    config = function()
      require('mini.align').setup()
    end
  },

  -- winbar
  {
    "ramilito/winbar.nvim",
    lazy = true,
    event = "VimEnter",
    config = function()
      require("winbar").setup({
        icons = true,
        diagnostics = true,
        buf_modified = true,
        buf_modified_symbol = "-",
        dir_level = 0,
        dim_inactive = {
          enabled = true,
          highlight = "WinbarNC",
          icons = false,
          name = true,
        },
        show_file_path = false,
        exclude_filetype = {
          "help",
          "terminal",
          "dashboard",
          "lazy",
          "fzf",
          "oil",
          "NvimTree",
          "Trouble",
          "alpha",
        },
      })
    end
  },
}
