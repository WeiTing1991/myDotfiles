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
      "williamboman/mason-lspconfig.nvim",
      "WhoIsSethDaniel/mason-tool-installer.nvim",
      { "j-hui/fidget.nvim", opt = {} },

      -- cmp
      { "saghen/blink.cmp" },
      -- {"jay-babu/mason-nvim-dap.nvim"},
    },
    config = function()
      require("lsp.lsp_init")
      -- Not sure
      -- vim.lsp.enable("copilot")
    end,
  },
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
        event = "InsertEnter",
        build = (function()
          if vim.fn.has("win32") == 1 or vim.fn.executable("make") == 0 then
            return
          end
          return "make install_jsregexp"
        end)(),
        -- dependencies = "rafamadriz/friendly-snippets",
        dependencies = {},
      --   opts = {
      --     history = true,
      --     delete_check_events = "TextChanged",
      --   },
      --   config = function(_, opts)
      --     require("luasnip").setup(opts)
      --     require("luasnip.loaders.from_vscode").lazy_load({ paths = { vim.fn.stdpath("config") .. "/snippets" } })
      --   end,
      },
    },
    config = function()
      require("plugins.configs.blink")
    end,
  },
}
