return {

  -- Markdwon preview
  -- NOTE: https://github.com/iamcco/markdown-preview.nvim
  {
    "iamcco/markdown-preview.nvim",
    event = "VeryLazy",
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
        -- style = "simple",
        hl = "MarkviewCode",
        sign = false,
      },
      html = {
        enable = false,
      },
    },
    -- config = function ()
    --   require("markview").setup()
    -- end
  },

  -- Markdown image
  {
    "3rd/image.nvim",
    event = "VeryLazy",
    opts = {},
    config = function()
      require "config.image"
    end,
  },

  -- -- Image pasting
  -- -- NOTE https://github.com/HakonHarnes/img-clip.nvim
  -- {
  --   "HakonHarnes/img-clip.nvim",
  --   enabled = false,
  --   event = "VeryLazy",
  --   ft = { "markdown" },
  --   opts = {
  --     -- recommended settings
  --     default = {
  --       embed_image_as_base64 = false,
  --       prompt_for_file_name = false,
  --       drag_and_drop = {
  --         insert_mode = true,
  --       },
  --       -- required for Windows users
  --       use_absolute_path = true,
  --     },
  --   },
  -- },
}
