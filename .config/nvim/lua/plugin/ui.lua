return {
  {
    "rebelot/heirline.nvim",
    event = "BufEnter",
    enabled = false,
    config = function()
      require "plugin.configs.heirline"
    end,
  },
  {
    "catgoose/nvim-colorizer.lua",
    event = "BufReadPre",
    opts = {},
    config = function()
      require("colorizer").setup {
        filetypes = {
          "*",
          "!vim",
          "!mason",
          "!lazy",
        },
        user_default_options = {
          names = false,
        },
      }
    end,
  },
  {
    "ramilito/winbar.nvim",
    lazy = true,
    event = "VimEnter",
    config = function()
      require("winbar").setup {
        icons = true,
        diagnostics = true,
        buf_modified = true,
        buf_modified_symbol = "M",
        dir_levels = 99, -- max levels of directories to show
        dim_inactive = {
          enabled = true,
          highlight = "WinbarNC",
          icons = false,
          name = true,
        },
        show_file_path = true,
        -- filetype_exclude = {
        --   "help",
        --   "tutor",
        --   "snacks_*",
        --   "terminal",
        --   "dashboard",
        --   "lazy",
        --   "fzf",
        --   "oil",
        --   "NvimTree",
        --   "Trouble",
        --   "alpha",
        -- },
      }
    end,
  },
}
