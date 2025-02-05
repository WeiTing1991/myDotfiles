return {
  -- -------------------------------------- Autocompletion --------------------------------------------------
  {
    "saghen/blink.cmp",
    lazy = true,
    event = "InsertEnter",
    dependencies = "rafamadriz/friendly-snippets",
    version = "*",
    config = function()
      require("configs.lsp.lsp-cmp")
    end,
  },
  {
    "L3MON4D3/LuaSnip",
    lazy = true,
    event = "InsertEnter",
    dependencies = "rafamadriz/friendly-snippets",
    version = "v2.*",
    build = (function()
      if vim.fn.has "win32" == 1 or vim.fn.executable "make" == 0 then
        return
      end
      return "make install_jsregexp"
    end)(),
    opts = {
      history = true,
      delete_check_events = "TextChanged",
      updateevents = "TextChanged,TextChangedI"
    },
    config = function(_, opts)
      require("luasnip").config.set_config(opts)
      -- require("luasnip.loaders.from_vscode").lazy_load { exclude = vim.g.vscode_snippets_exclude or {} }
      -- require("luasnip.loaders.from_vscode").lazy_load { paths = vim.g.vscode_snippets_path or "" }
      -- require "nvchad.configs.luasnip"
    end,
  },

  -- ------------------------------------- LSP config -------------------------------------------------------
  {
    "folke/lazydev.nvim",
    lazy = true,
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
    lazy = true,
    event = "VimEnter",
    dependencies = {
      -- NOTE: Must be loaded before dependants
      { "williamboman/mason.nvim", config = true },
      "williamboman/mason-lspconfig.nvim",
      "WhoIsSethDaniel/mason-tool-installer.nvim",
      { "j-hui/fidget.nvim",       opt = {} },

      -- cmp
      { "saghen/blink.cmp" },
      -- "hrsh7th/cmp-nvim-lsp",
      -- {"jay-babu/mason-nvim-dap.nvim"},
    },
    config = function()
      require("configs.lsp.lsp-init")         -- lsp engine
      require("configs.lsp.configs.keymaps")  -- lsp keymap
      --only if load with lspconfig and mason
      -- require "lsp.dap-init"
    end,
  },

  -- LSP saga
  {
    "nvimdev/lspsaga.nvim",
    lazy = true,
    -- enabled = false,
    event = "BufEnter",
    config = function()
      require("configs.lsp.lsp-ui")
    end,
  },

  -- formater and linter
  {
    "nvimtools/none-ls.nvim",
    lazy = true,
    event = "BufEnter",
    dependencies = {
      "nvimtools/none-ls-extras.nvim",
    },
    config = function()
      require ("configs.lsp.lsp-format")
    end,
  },

  -- others
  {
    "danymat/neogen",
    lazy = true,
    event = "BufEnter",
    -- version = "*"
    config = function()
      require("neogen").setup { snippet_engine = "luasnip" }
    end,
  },
  {
    "windwp/nvim-ts-autotag",
    lazy = true,
    event = "BufRead",
    ft = { "html", "javascript", "typescript", "javascriptreact", "typescriptreact", "svelte", "vue" },
    config = function()
      require("nvim-ts-autotag").setup({
        opts = {
          enable_close = true,          -- Auto close tags
          enable_rename = true,         -- Auto rename pairs of tags
          enable_close_on_slash = false -- Auto close on trailing </
        },
        per_filetype = {
          ["html"] = {
            enable_close = false
          }
        }
      })
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
}
