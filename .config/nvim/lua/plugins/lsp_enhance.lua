return {
  --[[ diagnostic hover ]]
  {
    "WeiTing1991/diagnostic-hover.nvim",
    lazy = true,
    event = "LspAttach",
    config = function()
      require("diagnostic-hover").setup({
        use_icons = false,
      })
    end,
  },
  --[[ Refactoring ]]
  {
    "ThePrimeagen/refactoring.nvim",
    lazy = true,
    event = "VeryLazy",
    opts = {},
    config = function()
      require("refactoring").setup({
        prompt_func_return_type = {
          go = false,
          java = false,

          cpp = false,
          c = false,
          h = false,
          hpp = false,
          cxx = false,
        },
        prompt_func_param_type = {
          go = false,
          java = false,

          cpp = false,
          c = false,
          h = false,
          hpp = false,
          cxx = false,
        },
        printf_statements = {},
        print_var_statements = {},
        show_success_message = true,
        -- shows a message with information about the refactor on success
        -- i.e. [Refactor] Inlined 3 variable occurrences
      })
    end,
  },

  -- [[Task Runner]]
  -- TODO
  {
    "stevearc/overseer.nvim",
    lazy = true,
    event = "VeryLazy",
    cmd = {
      "OverseerOpen",
      "OverseerClose",
      "OverseerToggle",
      "OverseerSaveBundle",
      "OverseerLoadBundle",
      "OverseerDeleteBundle",
      "OverseerRunCmd",
      "OverseerRun",
      "OverseerInfo",
      "OverseerBuild",
      "OverseerQuickAction",
      "OverseerTaskAction",
      "OverseerClearCache",
    },
    opts = {
      dap = false,
    },
    config = function()
      require("overseer").setup({
        templates = { "builtin", "lua.format", "csharp.format" },
      })
    end,
  },

  --[[ LANGUAGE ]]
  -- Json
  {
    "b0o/schemastore.nvim",
    lazy = true,
    ft = { "json" },
    events = "VeryLazy",
  },
  -- C#
  -- https://github.com/seblyng/roslyn.nvim
  {
    "seblyng/roslyn.nvim",
    ft = "cs",
    ---@module 'roslyn.config'
    ---@type RoslynNvimConfig
    opts = {},
  },

  -- C/C++
  {
    "p00f/clangd_extensions.nvim",
    lazy = true,
    ft = { "c", "cpp", "objc", "objcpp" },
    opts = {
      inlay_hints = {
        inline = false,
      },
      ast = {
        --These require codicons (https://github.com/microsoft/vscode-codicons)
        role_icons = {
          type = "",
          declaration = "",
          expression = "",
          specifier = "",
          statement = "",
          ["template argument"] = "",
        },
        kind_icons = {
          Compound = "",
          Recovery = "",
          TranslationUnit = "",
          PackExpansion = "",
          TemplateTypeParm = "",
          TemplateTemplateParm = "",
          TemplateParamObject = "",
        },
      },
    },
  },

  -- -- ts/js
  -- {
  --   "JoosepAlviste/nvim-ts-context-commentstring",
  --   lazy = true,
  --   event = "BufReadPre",
  --   ft = { "html", "javascript", "typescript", "javascriptreact", "typescriptreact", "svelte", "vue" },
  --   config = function()
  --     require("ts_context_commentstring").setup({
  --       enable_autocmd = false,
  --     })
  --     require("Comment").setup({
  --       pre_hook = require("ts_context_commentstring.integrations.comment_nvim").create_pre_hook(),
  --     })
  --   end,
  -- },
  --
  -- -- {
  -- --   "pmizio/typescript-tools.nvim",
  -- --   lazy = true,
  -- --   enabled = false,
  -- --   event = "BufReadPre",
  -- --   ft = { "html", "javascript", "typescript", "javascriptreact", "typescriptreact", "svelte", "vue" },
  -- -- },
  -- -- {
  -- --   "windwp/nvim-ts-autotag",
  -- --   lazy = true,
  -- --   enabled = false,
  -- --   event = "BufReadPre",
  -- --   ft = { "html", "javascript", "typescript", "javascriptreact", "typescriptreact", "svelte", "vue" },
  -- --   config = function()
  -- --     require("nvim-ts-autotag").setup({
  -- --       opts = {
  -- --         enable_close = false, -- Auto close tags
  -- --         enable_rename = true, -- Auto rename pairs of tags
  -- --         enable_close_on_slash = false, -- Auto close on trailing </
  -- --       },
  -- --       per_filetype = {
  -- --         ["html"] = {
  -- --           enable_close = false,
  -- --         },
  -- --       },
  -- --     })
  -- --   end,
  -- -- },
}
