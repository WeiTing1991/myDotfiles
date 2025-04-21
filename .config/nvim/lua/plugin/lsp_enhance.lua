return {
  -- [[Task Runner]]
  {
    "stevearc/overseer.nvim",
    lazy = true,
    key = {
      {
        "<leader>ot",
        "<cmd>OverseerToggle<cr>",
        desc = "Toggle task window",
      },
    },
  },

  --[[ language extra ]]
  -- ts/js
  {
    "pmizio/typescript-tools.nvim",
    lazy = true,
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
  --
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
