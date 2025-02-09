return {

  -- Markdwon preview
  -- NOTE: https://github.com/iamcco/markdown-preview.nvim
  {
    "iamcco/markdown-preview.nvim",
    lazy = true,
    event = "VeryLazy",
    cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
    ft = { "markdown"},
    build = "cd app && npm install",
    init = function()
      vim.g.mkdp_filetypes = { "markdown" }
    end,
  },
  {
    "toppair/peek.nvim",
    lazy = true,
    event = "VeryLazy" ,
    build = "deno task --quiet build",
    config = function()
      local app = 'webview'
      if vim.fn.has "win32" == 1 then
        app = '/mnt/c/Program Files/Google/Chrome/Application/chrome.exe'
      end
      require("peek").setup {
        filetype = { "markdown"},
          app = app
      }
      vim.api.nvim_create_user_command("PeekOpen", require("peek").open, {})
      vim.api.nvim_create_user_command("PeekClose", require("peek").close, {})
    end,
  },

  -- Markdown pretty render
  -- NOTE: https://github.com/OXY2DEV/markview.nvim/wiki
  -- {
  --   "OXY2DEV/markview.nvim",
  --   enabled = false,
  --   lazy = false, -- Recommended
  --   -- ft = "markdown" -- If you decide to lazy-load anyway
  --   dependencies = {
  --     "nvim-treesitter/nvim-treesitter",
  --   },
  --   opts = {
  --     modes = { "n", "nc", "no", "c", "t", "i" },
  --     hybrid_modes = { "i", "v" },
  --     headings = {
  --       shift_width = 2,
  --       heading_1 = {
  --         style = "simple",
  --       },
  --       heading_2 = {
  --         style = "simple",
  --       },
  --       heading_3 = {
  --         style = "simple",
  --       },
  --       heading_4 = {
  --         style = "simple",
  --       },
  --     },
  --     code_blocks = {
  --       enable = true,
  --       style = "simple",
  --       hl = "MarkviewCode",
  --       sign = false,
  --     },
  --     html = {
  --       enable = false,
  --     },
  --   },
  --   config = function ()
  --     require("markview").setup()
  --   end
  -- },

  -- Markdown image
  -- {
  --   "3rd/image.nvim",
  --   enabled = false,
  --   event = "VeryLazy",
  --   opts = {},
  --   config = function()
  --     require "config.image"
  --   end,
  -- },

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
