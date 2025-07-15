return {
  {
    "nanozuki/tabby.nvim",
    event = "VimEnter",
    -- config = function()
    -- end,
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
