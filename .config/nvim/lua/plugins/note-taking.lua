return {
  -- Markdwon preview
  -- NOTE: https://github.com/iamcco/markdown-preview.nvim
  {
    "iamcco/markdown-preview.nvim",
    event = "BufEnter",
    cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
    ft = { "markdown", ".md" },
    build = "cd app && npm install",
    init = function()
      vim.g.mkdp_filetypes = { "markdown" }
    end,
  },

  -- Markdown pretty render
  -- NOTE: https://github.com/OXY2DEV/markview.nvim/wiki
  {
    "OXY2DEV/markview.nvim",
    lazy = false, -- Recommended
    -- ft = "markdown" -- If you decide to lazy-load anyway
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
    },
    opts = {
      modes = { "n", "nc", "no", "c", "i", "t" },
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
    },
    -- config = function ()
    --   require("markview").setup()
    -- end
  },
  {
    "3rd/image.nvim",
    event = "BufEnter",
    opts = {},
    config = function()
      require("config.image")
    end,
  },
}
