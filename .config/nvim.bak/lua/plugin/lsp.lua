return {
  --[[ AUTOCOMPLETION ]]
  {
    "saghen/blink.cmp",
    lazy = true,
    event = "InsertEnter",
    dependencies = "rafamadriz/friendly-snippets",
    version = "*",
    config = function()
      require "configs.lsp.lsp-cmp"
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
      require("luasnip.loaders.from_vscode").lazy_load()
      -- require("luasnip.loaders.from_vscode").lazy_load({ paths = { vim.fn.stdpath("config") .. "/snippets" } })
    end,
  },

  --[[ LSP CONFIG ]]
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
      require "configs.lsp.lsp-init" -- lsp engine
      -- only if load with lspconfig and mason
      -- require "lsp.dap-init"
    end,
  },

  -- better fold
  {
    "kevinhwang91/nvim-ufo",
    lazy = true,
    event = "VeryLazy",
    dependencies = {
      "kevinhwang91/promise-async",
    },
    opts = {
      filetype_exclude = { "help", "alpha", "dashboard", "nvim-tree", "Trouble", "lazy", "mason" },
      provider_selector = function(_, _, _)
        return { "treesitter", "indent" }
      end,
      open_fold_hl_timeout = 0, -- Disable highlight timeout after opening
    },
    config = function(_, opts)
      vim.api.nvim_create_autocmd("FileType", {
        group = vim.api.nvim_create_augroup("local_detach_ufo", { clear = true }),
        pattern = opts.filetype_exclude,
        callback = function()
          require("ufo").detach()
        end,
      })

      vim.o.foldenable = true
      vim.o.foldcolumn = "0"
      vim.o.foldlevel = 99
      vim.o.foldlevelstart = 99
      require("ufo").setup(opts)
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
      require "configs.lsp.lsp-format"
    end,
  },

  -- others
  {
    "danymat/neogen",
    lazy = true,
    event = "InsertEnter",
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
      require "configs.lsp.lsp-ui"
    end,
  },

  --[[ language extra ]]
  -- ts/js
  {
    "windwp/nvim-ts-autotag",
    lazy = true,
    event = "BufReadPre",
    ft = { "html", "javascript", "typescript", "javascriptreact", "typescriptreact", "svelte", "vue" },
    config = function()
      require("nvim-ts-autotag").setup {
        opts = {
          enable_close = false,           -- Auto close tags
          enable_rename = true,          -- Auto rename pairs of tags
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
    "JoosepAlviste/nvim-ts-context-commentstring",
    lazy = true,
    event = "BufReadPre",
    opts = { enable_autocmd = false },
  },

  -- dart/flutter
  {
    "akinsho/flutter-tools.nvim",
    lazy = true,
    ft = { "dart" },
    dependencies = {
      "nvim-lua/plenary.nvim",
      "stevearc/dressing.nvim",
    },
    config = function()
      require("flutter-tools").setup {}
    end,
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
