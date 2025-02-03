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
    event = "VimEnter",
    config = function()
      require "configs.indentscope"
    end,
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    lazy = true,
    event = "VimEnter",
    main = "ibl",
    ---@module "ibl"
    ---@type ibl.config
    opts = {},
    config = function()
      require("ibl").setup {
        exclude = {
          filetypes = { "markdown" },
          buftypes = { "terminal" , "oil"},
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
    event = "BufEnter",
    opts = {
      char = { "┆" },
      virtcolumn = "110",
      highlight = { "NonText" },
      exclude = { filetypes = { "oil", "markdown" } },
    },
  },

  -- Better Comment
  {
    "numToStr/Comment.nvim",
    lazy = true,
    event = "InsertEnter",
    opts = {},
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

  -- winbar
  {
    "ramilito/winbar.nvim",
    lazy = true,
    event = "VimEnter",
    config = function()
      require("winbar").setup({
        icons = false,
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
        show_file_path = true,
        exclude_filetype = {
          "help",
          "startify",
          "terminal",
          "dashboard",
          "lazy",
          "fzf",
          "NvimTree",
          "Trouble",
          "alpha",
        },
      })
    end
  },
}
