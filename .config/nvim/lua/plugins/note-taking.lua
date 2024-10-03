return {
  -- note taking/markdown
  {
    "MeanderingProgrammer/render-markdown.nvim",
    opts = {
      file_types = { "markdown", "Avante" },
    },
    ft = { "markdown", "Avante" },
    dependencies = { "nvim-treesitter/nvim-treesitter", "echasnovski/mini.icons" },
    config = function()
      require("render-markdown").setup {}
    end,
  },
  {
    "iamcco/markdown-preview.nvim",
    lazy = false,
    event = "BufEnter",
    cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
    ft = { "markdown", ".md" },
    build = "cd app && npm install",
    init = function()
      vim.g.mkdp_filetypes = { "markdown" }
    end,
  },
  {
    "epwalsh/obsidian.nvim",
    version = "*",
    lazy = true,
    ft = "markdown",
    dependencies = {
      "nvim-lua/plenary.nvim",
    },
    opts = {},
    config = function()
      require "config.obsidian"
    end,
  },

  -- support for image pasting
  -- NOTE https://github.com/HakonHarnes/img-clip.nvim
  {
    "HakonHarnes/img-clip.nvim",
    enabled = false,
    event = "VeryLazy",
    ft = { "markdown" },
    opts = {
      -- recommended settings
      default = {
        embed_image_as_base64 = false,
        prompt_for_file_name = false,
        drag_and_drop = {
          insert_mode = true,
        },
        -- required for Windows users
        use_absolute_path = true,
      },
    },
  },
  {
    "vhyrro/luarocks.nvim",
    priority = 1001, -- this plugin needs to run before anything else
    opts = {
      rocks = { "magick" },
    },
  },
  {
    "3rd/image.nvim",
    enabled = false,
    config = function()
      require("image").setup {
        integrations = {
          markdown = {
            enabled = true,
            clear_in_insert_mode = false,
            download_remote_images = true,
            only_render_image_at_cursor = true,
            filetypes = { "markdown", "vimwiki" }, -- markdown extensions (ie. quarto) can go here
            resolve_image_path = function(document_path, image_path, fallback)
              -- document_path is the path to the file that contains the image
              -- image_path is the potentially relative path to the image. for
              -- markdown it's `![](this text)`

              -- you can call the fallback function to get the default behavior
              return fallback(document_path, image_path)
            end,
          },
        },
      }
    end,
  },

  {
    "yetone/avante.nvim",
    opts = {},
    enabled = false,
    build = "make",
    dependencies = {
      "stevearc/dressing.nvim",
      "nvim-lua/plenary.nvim",
      "MunifTanjim/nui.nvim",
      --- The below dependencies are optional,
      "nvim-tree/nvim-web-devicons", -- or echasnovski/mini.icons
    },
  },
}

--[[
Leaderaa	show sidebar
Leaderar	refresh sidebar
Leaderae	edit selected blocks
co	choose ours
ct	choose theirs
ca	choose all theirs
c0	choose none
cb	choose both
cc	choose cursor
]x	move to previous conflict
[x	move to next conflict
[[	jump to previous codeblocks (results window)
]]
-- jump to next codeblocks (results windows)
-- ]]
