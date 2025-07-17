return {
  -- {
  --   "stevearc/aerial.nvim",
  --   lazy = true,
  --   event = "VeryLazy",
  --   opts = function()
  --     local icons = vim.deepcopy(require("icon").symbol_kinds)
  --     local opts = {
  --       attach_mode = "global",
  --       backends = { "treesitter", "lsp", "markdown", "man" },
  --       show_guides = true,
  --       layout = {
  --         max_width = { 60, 0.3 },
  --         width = nil,
  --         min_width = 30,
  --         resize_to_content = true,
  --         win_opts = {
  --           -- winhl = "Normal:NormalFloat,FloatBorder:NormalFloat,SignColumn:SignColumnSB",
  --           signcolumn = "yes",
  --           statuscolumn = " ",
  --         },
  --       },
  --       icons = vim.o.filetype == "markdown" and {} or icons,
  --       guides = {
  --         mid_item = "├╴",
  --         last_item = "└╴",
  --         nested_top = "│ ",
  --         whitespace = "  ",
  --       },
  --     }
  --     return opts
  --   end,
  --   keys = {
  --     { "<S-l>o", "<cmd>AerialToggle<cr>", desc = "Aerial (Symbols)" },
  --   },
  -- },

  -- {
  --   "rmagatti/goto-preview",
  --   dependencies = { "rmagatti/logger.nvim" },
  --   event = "BufEnter",
  --   config = true,
  -- },

  {
    "danymat/neogen",
    lazy = true,
    event = "VeryLazy",
    config = function()
      require("neogen").setup({
        enabled = true,
      })
    end,
  },

  --[[ LANGUAGE ]]
  -- Json
  {
    "b0o/schemastore.nvim",
    lazy = true,
    ft = "jsonc",
    events = "VeryLazy",
  },

  -- ts/js
  {
    "JoosepAlviste/nvim-ts-context-commentstring",
    lazy = true,
    event = "BufReadPre",
    ft = { "html", "javascript", "typescript", "javascriptreact", "typescriptreact", "svelte", "vue" },
    config = function()
      require("ts_context_commentstring").setup({
        enable_autocmd = false,
      })
      require("Comment").setup({
        pre_hook = require("ts_context_commentstring.integrations.comment_nvim").create_pre_hook(),
      })
    end,
  },

  -- NOTE: https://github.com/iabdelkareem/csharp.nvim
  -- C#
  {
    "Hoffs/omnisharp-extended-lsp.nvim",
    lazy = true,
    ft = { "cs" },
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

  -- [[Task Runner]]
  -- {
  --   "stevearc/overseer.nvim",
  --   lazy = true,
  --   enabled = false,
  --   key = {
  --     {
  --       "<leader>ot",
  --       "<cmd>OverseerToggle<cr>",
  --       desc = "Toggle task window",
  --     },
  --   },
  -- },

  -- {
  --   "pmizio/typescript-tools.nvim",
  --   lazy = true,
  --   enabled = false,
  --   event = "BufReadPre",
  --   ft = { "html", "javascript", "typescript", "javascriptreact", "typescriptreact", "svelte", "vue" },
  -- },
  -- {
  --   "windwp/nvim-ts-autotag",
  --   lazy = true,
  --   enabled = false,
  --   event = "BufReadPre",
  --   ft = { "html", "javascript", "typescript", "javascriptreact", "typescriptreact", "svelte", "vue" },
  --   config = function()
  --     require("nvim-ts-autotag").setup({
  --       opts = {
  --         enable_close = false, -- Auto close tags
  --         enable_rename = true, -- Auto rename pairs of tags
  --         enable_close_on_slash = false, -- Auto close on trailing </
  --       },
  --       per_filetype = {
  --         ["html"] = {
  --           enable_close = false,
  --         },
  --       },
  --     })
  --   end,
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
