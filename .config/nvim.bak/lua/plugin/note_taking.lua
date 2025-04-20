return {
  {
    "OXY2DEV/markview.nvim",
    enabled = true,
    lazy = true,
    ft = "markdown",
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
    },
    opts = {
      modes = { "n", "nc", "no", "c", "t", "i" },
      hybrid_modes = { "i", "v" },
      headings = {
        shift_width = 2,
        heading_1 = {
          style = "simple",
        },
        heading_2 = {
          style = "simple",
        },
        heading_3 = {
          style = "simple",
        },
        heading_4 = {
          style = "simple",
        },
      },
      code_blocks = {
        enable = true,
        style = "simple",
        hl = "MarkviewCode",
        sign = false,
      },
      html = {
        enable = false,
      },
    },
    config = function()
      require("markview").setup()
    end,
  },

}
