return {
  {
    "theprimeagen/vim-be-good",
    event = "VeryLazy",
    dependencies = {
      "nvim-lua/plenary.nvim",
    },

    config = function() end,
  },
  {
    "stevearc/oil.nvim",
    opts = {
      default_file_explorer = false,
      keymaps = {
        ["g?"] = "actions.show_help",
        ["<CR>"] = "actions.select",
        ["<C-s>"] = "",
        ["<C-h>"] = "",
        ["<C-t>"] = "",
        ["<C-p>"] = "actions.preview",
        ["<C-c>"] = "actions.close",
        ["<C-l>"] = "actions.refresh",
        ["-"] = "actions.parent",
        ["_"] = "actions.open_cwd",
        ["`"] = "actions.cd",
        ["~"] = "actions.tcd",
        ["gs"] = "actions.change_sort",
        ["gx"] = "actions.open_external",
        ["g."] = "actions.toggle_hidden",
        ["g\\"] = "actions.toggle_trash",
      },
      float = {
        -- Padding around the floating window
        padding = 10,
        max_width = 0,
        max_height = 0,
        border = "rounded",
        win_options = {
          winblend = 0,
        },
      },
      view_options = {
        show_hidden = true,
      },
    },
    -- Optional dependencies
    dependencies = { "nvim-tree/nvim-web-devicons" },
  },
  {
    "p00f/clangd_extensions.nvim",
    lazy = true,
    config = function() end,
    opts = {
      inlay_hints = {
        inline = false,
      },
      ast = {
        --These require codicons (https://github.com/microsoft/vscode-codicons)
        role_icons = {
          type = "",
          declaration = "",
          expression = "",
          specifier = "",
          statement = "",
          ["template argument"] = "",
        },
        kind_icons = {
          Compound = "",
          Recovery = "",
          TranslationUnit = "",
          PackExpansion = "",
          TemplateTypeParm = "",
          TemplateTemplateParm = "",
          TemplateParamObject = "",
        },
      },
    },
  },
}
