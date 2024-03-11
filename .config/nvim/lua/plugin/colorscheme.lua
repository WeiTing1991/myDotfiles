return {
  {
    "aktersnurra/no-clown-fiesta.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      require("no-clown-fiesta").setup({
        transparent = true, -- Enable this to disable the bg color
        styles = {
          -- You can set any of the style values specified for `:h nvim_set_hl`
          comments = {},
          keywords = {},
          functions = {},
          variables = {},
          type = { bold = true },
          lsp = { underline = true },
        },
      })
      --vim.cmd.colorscheme("no-clown-fiesta")
    end,
  },
  { -- https://github.com/catppuccin/nvim
    "catppuccin/nvim",
    name = "catppuccin",
    lazy = false,
    priority = 1000,
    config = function()
      require("catppuccin").setup({
        flavour = "macchiato", -- latte, frappe, macchiato, mocha
        background = { -- :h background
          light = "latte",
          dark = "mocha",
        },
        transparent_background = true, -- disables setting the background color.
        show_end_of_buffer = false, -- shows the '~' characters after the end of buffers
        term_colors = true, -- sets terminal colors (e.g. `g:terminal_color_0`)
        dim_inactive = {
          enabled = false, -- dims the background color of inactive window
          shade = "dark",
          percentage = 0.3, -- percentage of the shade to apply to the inactive window
        },
        no_italic = true, -- Force no italic
        no_bold = true, -- Force no bold
        no_underline = false, -- Force no underline
        styles = { -- Handles the styles of general hi groups (see `:h highlight-args`):
          comments = { "italic" }, -- Change the style of comments
          conditionals = { "italic" },
          loops = {},
          functions = {},
          keywords = {},
          strings = {},
          variables = {},
          numbers = {},
          booleans = {},
          properties = {},
          types = {},
          operators = {},
          -- miscs = {}, -- Uncomment to turn off hard-coded styles
        },
        color_overrides = {},
        custom_highlights = {},
        integrations = {
          cmp = true,
          telescope = {
            enabled = true,
            style = "nvchad",
          },
          harpoon = true,
          gitsigns = true,
          nvimtree = true,
          treesitter = true,
          mini = {
            enabled = true,
            indentscope_color = "",
          },
          highlight_overrides = {
            all = function(colors)
              return {
                NvimTreeNormal = { fg = colors.none },
                CmpBorder = { fg = "#3e4145" },
              }
            end,
          },
          -- For more plugins integrations please scroll down (https://github.com/catppuccin/nvim#integrations)
        },
      })
      --vim.cmd.colorscheme("catppuccin")
    end,
  },
  {
    "rose-pine/neovim",
    name = "rose-pine",
    lazy = false,
    priority = 1000,
    -- https://rosepinetheme.com/palette/ingredients/
    config = function()
      require("rose-pine").setup({
        variant = "moon", -- auto, main, moon, or dawn
        dark_variant = "main", -- main, moon, or dawn
        dim_inactive_windows = false,
        extend_background_behind_borders = true,

        bold_vert_split = true,

        enable = {
          terminal = true,
          legacy_highlights = true, -- Improve compatibility for previous versions of Neovim
          migrations = true, -- Handle deprecated options automatically
        },

        styles = {
          bold = true,
          italic = false,
          transparency = true,
        },

        groups = {
          comment = "muted",
          punctuation = "subtle",
          border = "Text",
          link = "iris",

          error = "love",
          hint = "iris",
          info = "foam",
          note = "pine",
          todo = "rose",
          warn = "gold",

          git_add = "foam",
          git_change = "rose",
          git_delete = "love",
          git_dirty = "rose",
          git_ignore = "muted",
          git_merge = "iris",
          git_rename = "pine",
          git_stage = "iris",
          git_text = "rose",
          git_untracked = "subtle",

          h1 = "iris",
          h2 = "foam",
          h3 = "rose",
          h4 = "gold",
          h5 = "pine",
          h6 = "foam",
        },

        highlight_groups = {
          -- treesitter
          -- Comment = { fg = "foam" },
          -- VertSplit = { fg = "muted", bg = "muted" },
          -- for treesitter highlights, they start with an @, so you'll have to do something like:
          -- ["@string"] = { fg = "rose" },
          -- ["@character"] = { fg = "rose" },
        },
      })

      vim.cmd("colorscheme rose-pine")
    end,
  },
}
