return {
  {
    "goolord/alpha-nvim",
    lazy = true,
    event = "VimEnter",
    config = function()
      require "plugin.configs.dashboard"
    end,
  },
  {
    "nvzone/volt",
    lazy = true,
    event = "VeryLazy",
  },
  {
    "nvzone/minty",
    lazy = true,
    event = "VeryLazy",
    cmd = { "Shades", "Huefy" },
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
      }
    end,
  },
  {
    "ramilito/winbar.nvim",
    lazy = true,
    event = "VimEnter",
    config = function()
      require("winbar").setup {
        icons = false,
        diagnostics = true,
        buf_modified = true,
        buf_modified_ssymbol = "M",
        dir_level = 2,
        dim_inactive = {
          enabled = true,
          highlight = "WinbarNC",
          icons = false,
          name = true,
        },
        show_file_path = true,
        filetype_exclude = {
          "help",
          "tutor",
          "terminal",
          "dashboard",
          "lazy",
          "fzf",
          "oil",
          "NvimTree",
          "Trouble",
          "alpha",
        },
      }
    end,
  },
}
