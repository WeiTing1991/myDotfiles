return {
  --[[ LSP CONFIG ]]
  {
    "folke/lazydev.nvim",
    lazy = true,
    ft = "lua",
    event = "VeryLazy",
    opts = {
      library = {
        { path = "${3rd}/luv/library", words = { "vim%.uv" } },
      },
    },
  },
  {
    "neovim/nvim-lspconfig",
    lazy = true,
    event = "VeryLazy",
    dependencies = {
      -- NOTE: Must be loaded before dependants
      { "williamboman/mason.nvim", opt = {} },
      -- "williamboman/mason-lspconfig.nvim",
      "WhoIsSethDaniel/mason-tool-installer.nvim",
      { "j-hui/fidget.nvim", opt = {} },

      -- cmp
      { "saghen/blink.cmp" },
      -- {"jay-babu/mason-nvim-dap.nvim"},
    },
    config = function()
      require("plugin.configs.lsp")
    end,
  },

  --[[ AUTOCOMPLETION ]]
  {
    "saghen/blink.cmp",
    lazy = true,
    event = "InsertEnter",
    dependencies = {
      {
        "L3MON4D3/LuaSnip",
        version = "2.*",
        event = "InsertEnter",
        build = (function()
          if vim.fn.has("win32") == 1 or vim.fn.executable("make") == 0 then
            return
          end
          return "make install_jsregexp"
        end)(),
        -- dependencies = "rafamadriz/friendly-snippets",
        dependencies = {},
        opts = {
          history = true,
          delete_check_events = "TextChanged",
        },
        config = function(_, opts)
          require("luasnip").setup(opts)
          require("luasnip.loaders.from_vscode").lazy_load({ paths = { vim.fn.stdpath("config") .. "/snippets" } })
        end,
      },
    },
    version = "1.*",
    config = function()
      require("plugin.configs.blink")
    end,
  },

  --[[ Formater/ Linter ]]
  {
    "WeiTing1991/none-ls.nvim",
    lazy = true,
    event = "BufEnter",
    dependencies = {
      "nvimtools/none-ls-extras.nvim",
    },
    config = function()
      require("plugin.configs.nonels")
    end,
  },

  --[[ FOMATER ]]
  -- {
  --   "stevearc/conform.nvim",
  --   lazy = true,
  --   event = { "BufWritePre" },
  --   cmd = { "ConformInfo" },
  --   config = function()
  --     local conform = require "conform"
  --     conform.setup {
  --       notify_on_error = false,
  --       -- format_on_save = false,
  --     }
  --     conform.formatters_by_ft = require "lsp.formater"
  --   end,
  -- },
}
