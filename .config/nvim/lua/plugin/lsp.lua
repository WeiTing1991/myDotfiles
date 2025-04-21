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
      require "plugin.configs.lsp" -- lsp engine
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
          if vim.fn.has "win32" == 1 or vim.fn.executable "make" == 0 then
            return
          end
          return "make install_jsregexp"
        end)(),
        -- dependencies = "rafamadriz/friendly-snippets",
        dependencies = {},
        opts = {
          history = true,
          -- delete_check_events = "TextChanged",
        },
        -- config = function(_, opts)
        --   require("luasnip").config.set_config(opts)
        --   require("luasnip.loaders.from_vscode").lazy_load()
        --   -- require("luasnip.loaders.from_vscode").lazy_load({ paths = { vim.fn.stdpath("config") .. "/snippets" } })
        -- end,
      },
    },
    version = "1.*",
    config = function()
      require "plugin.configs.blink"
    end,
  },

  --[[ FOMATER ]]
  {
    "stevearc/conform.nvim",
    lazy = true,
    event = { "BufWritePre" },
    cmd = { "ConformInfo" },
    config = function()
      local conform = require "conform"
      conform.setup {
        notify_on_error = false,
        -- format_on_save = false,
      }
      conform.formatters_by_ft = require "lsp.formater"
    end,
  },
  -- {
  --   "mfussenegger/nvim-lint",
  --   lazy = true,
  --   event = { "BufEnter" },
  --   config = function()
  --     local lint = require("lint")
  --
  --     vim.env.ESLINT_D_PPID = vim.fn.getpid()
  --     lint.linters_by_ft = require("lsp.linter")
  --
  --     local lint_augroup = vim.api.nvim_create_augroup("wtc/lint", { clear = true })
  --     vim.api.nvim_create_autocmd({"BufWritePost" }, {
  --       group = lint_augroup,
  --       callback = function()
  --         lint.try_lint()
  --       end,
  --     })
  --
  --   end,
  -- },

  --[[ linter ]]
  {
    "nvimtools/none-ls.nvim",
    lazy = true,
    event = "BufEnter",
    dependencies = {
      "nvimtools/none-ls-extras.nvim",
    },
    config = function()
      local null_ls = require "null-ls"
      local null_ls_utils = require "null-ls.utils"
      -- local formatting = null_ls.builtins.formatting
      null_ls.setup {
        debug = false,
        root_dir = null_ls_utils.root_pattern(".null-ls-root", "Makefile", ".git", "package.json"),
        sources = {
          -- spell
          null_ls.builtins.completion.spell,
          -- null_ls.builtins.diagnostics.write_good,
          require("none-ls.diagnostics.eslint_d").with {
            -- root_markers = { ".eslintrc", ".eslintrc.js", ".eslintrc.json", "eslint.config.js", "eslint.config.mjs" },
            -- filetypes = { "javascript", "javascriptreact", "typescript", "typescriptreact", "graphql" },
            -- args = {
            --   "--no-warn-ignored", -- <-- this is the key argument
            --   "--format",
            --   "json",
            --   "--stdin",
            --   "--stdin-filename",
            --   function()
            --     return vim.api.nvim_buf_get_name(0)
            --   end,
            -- },
            -- before_init = function(params, config)
            --   -- Set the workspace folder setting for correct search of tsconfig.json files etc.
            --   config.settings.workspaceFolder = {
            --     uri = params.rootPath,
            --     name = vim.fn.fnamemodify(params.rootPath, ":t"),
            --   }
            -- end,
          },
        },
      }
    end,
  },

  --[[ LANGUAGE ]]
  -- Json
  {
    "b0o/schemastore.nvim",
    lazy = true,
    events = "VeryLazy",
  },
}
