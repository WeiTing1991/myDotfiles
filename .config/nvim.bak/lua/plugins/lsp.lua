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
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      -- NOTE: Must be loaded before dependants
      { "williamboman/mason.nvim", opt = {} },
      "WhoIsSethDaniel/mason-tool-installer.nvim",
      { "j-hui/fidget.nvim", opt = {} },
      -- cmp
      { "saghen/blink.cmp" },
    },
    config = function()
      require("lsp.lsp_init")
    end,
  },

  { import = "nvchad.blink.lazyspec" },
  --[[ AUTOCOMPLETION ]]
  {
    "saghen/blink.cmp",
    lazy = true,
    event = "InsertEnter",
    version = "1.*",
    dependencies = {
      {
        "L3MON4D3/LuaSnip",
        version = "2.*",
        lazy = true,
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
          local snippet_path = vim.fn.stdpath("config") .. "/snippets"
          require("luasnip.loaders.from_vscode").lazy_load({ paths = {
            snippet_path,
          } })
          -- Debug
          -- vim.defer_fn(function()
          --   local snips = require("luasnip").get_snippets("python")
          --   print("Snippets loaded: " .. vim.inspect(vim.tbl_keys(snips or {})))
          -- end, 100)
        end,
      },
    },
    config = function()
      require("plugins.configs.blink")
    end,
  },
  --[[ Formater/ Linter ]]
  {
    "WeiTing1991/none-ls.nvim",
    lazy = true,
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      "nvimtools/none-ls-extras.nvim",
    },
    config = function()
      require("plugins.configs.nonels")
    end,
  },
}
