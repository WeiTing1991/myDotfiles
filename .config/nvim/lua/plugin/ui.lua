return {
  -- Statusline
  {
    "WeiTing1991/staline.nvim",
    priority = 1000,
    lazy = false,
    config = function()
      require "plugin.configs.staline"
    end,
  },
  {
    'nanozuki/tabby.nvim',
    ---@type TabbyConfig
    opts = {
      -- configs...
    },
  },
  {
    "catgoose/nvim-colorizer.lua",
    event = "BufReadPre",
    opts = {},
    config = function()
      require("colorizer").setup({
        filetypes = {
          "*",
          "!vim",
          "!mason",
          "!lazy",
        },
        user_default_options = {
          names = false,
        },
      })
    end,
  },
}
