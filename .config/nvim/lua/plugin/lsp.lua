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
      -- updateevents = "TextChanged,TextChangedI"
    },
    config = function(_, opts)
      require("luasnip").config.set_config(opts)
      -- require "nvchad.configs.luasnip"
      require("luasnip.loaders.from_vscode").lazy_load()
      require("luasnip.loaders.from_vscode").lazy_load({ paths = { vim.fn.stdpath("config") .. "/snippets" } })
    end,
  },


  -- ------------------------------------- LSP config -------------------------------------------------------
  {
    "folke/lazydev.nvim",
    lazy = true,
    ft = "lua",
    event = "VeryLazy",
    opts = {
      library = {
        -- Load luvit types when the `vim.uv` word is found
        { path = "${3rd}/luv/library", words = { "vim%.uv" } },
      },
    },
  },
  {
    "Bilal2453/luvit-meta",
    lazy = true,
    ft = "lua",
    event = "VeryLazy",
  },
  {
    "neovim/nvim-lspconfig",
    lazy = true,
    event = "VeryLazy",
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
      require("configs.lsp.lsp-init") -- lsp engine
      -- only if load with lspconfig and mason
      -- require "lsp.dap-init"
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
      require("configs.lsp.lsp-format")
    end,
  },

  -- others
  {
    "danymat/neogen",
    lazy = true,
    event = "InsertEnter",
    -- version = "*"
    config = function()
      require("neogen").setup { snippet_engine = "luasnip" }
    end,
  },

  -- CHECK: https://github.com/hedyhli/outline.nvim
  -- UI
  {
    "hedyhli/outline.nvim",
    lazy = true,
    event = "VeryLazy",
    cmd = "Outline",
    opts = function()
      local defaults = require("outline.config").defaults
      local opts = {
        keymaps = {
          up_and_jump = "<up>",
          down_and_jump = "<down>",
        },
        -- symbols = {
        --   icon_fetcher = function(kind, bufnr, symbol) return kind:sub(1, 1) end,
        -- },
        -- symbol_folding = {
        --   autofold_depth = 1,
        --   auto_unfold = {
        --     hovered = true,
        --   },
        -- },
        -- preview_window = {
        --   auto_preview = true,
        -- },
      }
      return opts
    end,
  },

  -- LSP saga
  {
    "nvimdev/lspsaga.nvim",
    lazy = true,
    event = "VeryLazy",
    config = function()
      require("configs.lsp.lsp-ui")
    end,
  },

  -- Lanugae extra
  -- ts/js
  {
    "windwp/nvim-ts-autotag",
    lazy = true,
    event = "BufReadPre",
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
  -- NOTE: https://github.com/pmizio/typescript-tools.nvim?tab=readme-ov-file
  -- CHECK:
  -- {
  --   "pmizio/typescript-tools.nvim",
  --   lazy = true,
  --   event = "BufReadPre",
  --   ft = { "html", "javascript", "typescript", "javascriptreact", "typescriptreact", "svelte", "vue" },
  --   config = function()
  --     require("typescript-tools").setup {
  --       on_attach =
  --           function(client, bufnr)
  --             client.server_capabilities.documentFormattingProvider = false
  --             client.server_capabilities.documentRangeFormattingProvider = false
  --           end,
  --       settings = {
  --         jsx_close_tag = {
  --           enable = false,
  --           filetypes = { "javascriptreact", "typescriptreact" },
  --         }
  --       }
  --     }
  --   end
  -- },
  {
    "JoosepAlviste/nvim-ts-context-commentstring",
    lazy = true,
    event = "BufReadPre",
    opts = { enable_autocmd = false, },
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
}
