return {
  {
    "rmagatti/goto-preview",
    dependencies = { "rmagatti/logger.nvim" },
    event = "BufEnter",
    config = true,
  },
  {
    "stevearc/aerial.nvim",
    lazy = true,
    event = "VeryLazy",
    opts = function()
      local icons = vim.deepcopy(require("icon").symbol_kinds)
      local opts = {
        attach_mode = "global",
        backends = { "lsp", "treesitter", "markdown", "man" },
        show_guides = true,
        layout = {
          resize_to_content = false,
          win_opts = {
            winhl = "Normal:NormalFloat,FloatBorder:NormalFloat,SignColumn:SignColumnSB",
            signcolumn = "yes",
            statuscolumn = " ",
          },
        },
        icons = icons,
        guides = {
          mid_item = "├╴",
          last_item = "└╴",
          nested_top = "│ ",
          whitespace = "  ",
        },
      }
      return opts
    end,
    keys = {
      { "<S-l>o", "<cmd>AerialToggle<cr>", desc = "Aerial (Symbols)" },
    },
  },

  -- [[Task Runner]]
  {
    "stevearc/overseer.nvim",
    lazy = true,
    enabled = false,
    key = {
      {
        "<leader>ot",
        "<cmd>OverseerToggle<cr>",
        desc = "Toggle task window",
      },
    },
  },

  --[[ language extra ]]
  -- C#
  {
    "Hoffs/omnisharp-extended-lsp.nvim",
    lazy = true,
    ft = { "cs" },
  },

  -- ts/js
  {
    "pmizio/typescript-tools.nvim",
    lazy = true,
    enabled = false,
    event = "BufReadPre",
    ft = { "css", "html", "javascript", "typescript", "javascriptreact", "typescriptreact", "svelte", "vue" },
    config = function()
      require("typescript-tools").setup {
        on_attach = function(client, bufnr)
          client.server_capabilities.documentFormattingProvider = false
          client.server_capabilities.documentRangeFormattingProvider = false
        end,
        settings = {
          jsx_close_tag = {
            enable = false,
            filetypes = { "javascriptreact", "typescriptreact" },
          },
        },
      }
    end,
  },
  {
    "windwp/nvim-ts-autotag",
    lazy = true,
    enabled = false,
    event = "BufReadPre",
    ft = { "html", "javascript", "typescript", "javascriptreact", "typescriptreact", "svelte", "vue" },
    config = function()
      require("nvim-ts-autotag").setup {
        opts = {
          enable_close = false, -- Auto close tags
          enable_rename = true, -- Auto rename pairs of tags
          enable_close_on_slash = false, -- Auto close on trailing </
        },
        per_filetype = {
          ["html"] = {
            enable_close = false,
          },
        },
      }
    end,
  },
  {
    "JoosepAlviste/nvim-ts-context-commentstring",
    lazy = true,
    enabled = false,
    event = "BufReadPre",
    ft = { "css", "html", "javascript", "typescript", "javascriptreact", "typescriptreact", "svelte", "vue" },
    opts = { enable_autocmd = false },
  },

  -- c/c++
  -- {
  --   "p00f/clangd_extensions.nvim",
  --   lazy = true,
  --   config = function() end,
  --   opts = {
  --     inlay_hints = {
  --       inline = false,
  --     },
  --   },
  -- },

  -- go
  -- dart/flutter
  -- {
  --   "akinsho/flutter-tools.nvim",
  --   lazy = true,
  --   ft = { "dart" },
  --   dependencies = {
  --     "nvim-lua/plenary.nvim",
  --     "stevearc/dressing.nvim",
  --   },
  --   config = function()
  --     require("flutter-tools").setup {}
  --   end,
  -- },
}
