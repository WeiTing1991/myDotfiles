return {

  -- --------------------------- Autocompeletion ---------------------------------
  {
    'saghen/blink.cmp',
    event = "InsertEnter",
    dependencies = 'rafamadriz/friendly-snippets',
    version = '*',
    config = function()
      require("lsp.cmp")
    end
  },

  -- --------------------------- LSP config --------------------------------------
  {
    "folke/lazydev.nvim",
    ft = "lua",
    opts = {
      library = {
        -- Load luvit types when the `vim.uv` word is found
        { path = "${3rd}/luv/library", words = { "vim%.uv" } },
      },
    },
  },
  { "Bilal2453/luvit-meta", lazy = true },
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      -- NOTE: Must be loaded before dependants
      { "williamboman/mason.nvim", config = true },
      "williamboman/mason-lspconfig.nvim",
      "WhoIsSethDaniel/mason-tool-installer.nvim",
      { "j-hui/fidget.nvim",       opt = {} },

      -- cmp
      { "saghen/blink.cmp" }
      -- "hrsh7th/cmp-nvim-lsp",
      -- {"jay-babu/mason-nvim-dap.nvim"},
    },
    config = function()
      require "lsp.lsp-init" -- lsp engine
      --only if load with lspconfig and mason
      -- require "lsp.dap-init"
    end,
  },

  -- LSP saga
  {
    "nvimdev/lspsaga.nvim",
    event = "BufEnter",
    config = function()
      require "lsp.lsp-ui"
    end,
  },

  -- formater and linter
  {
    "nvimtools/none-ls.nvim",
    dependencies = {
      "nvimtools/none-ls-extras.nvim",
    },
    event = "BufEnter",
    config = function()
      require "lsp.lsp-format"
    end,
  },

  -- -- debugger
  -- {
  --   "mfussenegger/nvim-dap",
  --   event = "BufEnter",
  --   dependencies = {
  --     "rcarriga/nvim-dap-ui",
  --     "theHamsta/nvim-dap-virtual-text",
  --     "jay-babu/mason-nvim-dap.nvim",
  --     "nvim-neotest/nvim-nio",
  --
  --     -- Add own debuggers here
  --     "leoluz/nvim-dap-go",
  --     "mfussenegger/nvim-dap-python",
  --   },
  -- },
  --
  -- -- other lsp
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
}
