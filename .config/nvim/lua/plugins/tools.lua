return {
  -- Diagnostics list
  {
    "folke/trouble.nvim",
    cmd = "Trouble",
    opts = {
      focus = true,
      auto_preview = true,
      auto_close = false,
      preview = {
        type = "float",
        relative = "editor",
        size = { width = 0.8, height = 0.5 },
        position = { 0.5, 0.8 },
        border = "single",
      },
    },
    keys = {
      { "<leader>xd", "<cmd>Trouble diagnostics toggle filter.buf=0<cr>", desc = "Buffer diagnostics" },
      { "<leader>xw", "<cmd>Trouble diagnostics toggle<cr>", desc = "Workspace diagnostics" },
      { "<leader>xq", "<cmd>Trouble qflist toggle<cr>", desc = "Quickfix list" },
      { "<leader>xl", "<cmd>Trouble loclist toggle<cr>", desc = "Location list" },
      { "]t", function() require("trouble").next({ skip_groups = true, jump = true }) end, desc = "Next trouble item" },
      { "[t", function() require("trouble").prev({ skip_groups = true, jump = true }) end, desc = "Previous trouble item" },
    },
  },

  -- Outline / document symbols
  {
    "hedyhli/outline.nvim",
    cmd = { "Outline", "OutlineOpen" },
    opts = {},
  },

  -- Refactoring
  {
    "ThePrimeagen/refactoring.nvim",
    dependencies = { "nvim-lua/plenary.nvim", "nvim-treesitter/nvim-treesitter" },
    keys = {
      { "<leader>re", ":Refactor extract ", mode = "x", desc = "Extract method" },
      { "<leader>rf", ":Refactor extract_to_file ", mode = "x", desc = "Extract to file" },
      { "<leader>rv", ":Refactor extract_var ", mode = "x", desc = "Extract variable" },
      { "<leader>ri", ":Refactor inline_var", mode = { "n", "x" }, desc = "Inline variable" },
      { "<leader>rI", ":Refactor inline_func", desc = "Inline function" },
      { "<leader>rb", ":Refactor extract_block", desc = "Extract block" },
      { "<leader>rbf", ":Refactor extract_block_to_file", desc = "Extract block to file" },
    },
    opts = {
      prompt_func_return_type = {
        go = false, java = false,
        cpp = false, c = false, h = false, hpp = false, cxx = false,
      },
      prompt_func_param_type = {
        go = false, java = false,
        cpp = false, c = false, h = false, hpp = false, cxx = false,
      },
      show_success_message = true,
    },
  },

  -- Task runner
  {
    "stevearc/overseer.nvim",
    cmd = { "OverseerRun", "OverseerOpen", "OverseerToggle", "OverseerInfo", "OverseerBuild" },
    keys = {
      { "<leader>tr", "<cmd>OverseerRun<cr>", desc = "Run task" },
    },
    opts = {
      dap = false,
      templates = { "builtin", "lua.format", "csharp.format", "csharp.build", "cpp.build" },
    },
  },

  -- C/C++ extensions
  {
    "p00f/clangd_extensions.nvim",
    ft = { "c", "cpp", "objc", "objcpp" },
    opts = {
      inlay_hints = { inline = false },
      ast = {
        role_icons = {
          type = "", declaration = "", expression = "",
          specifier = "", statement = "", ["template argument"] = "",
        },
        kind_icons = {
          Compound = "", Recovery = "", TranslationUnit = "",
          PackExpansion = "", TemplateTypeParm = "",
          TemplateTemplateParm = "", TemplateParamObject = "",
        },
      },
    },
  },

  -- Python venv selector
  {
    "linux-cultist/venv-selector.nvim",
    ft = "python",
    opts = {},
  },

  -- C# (Roslyn)
  {
    "seblyng/roslyn.nvim",
    ft = "cs",
    opts = {},
  },

  -- Markdown preview
  {
    "WeiTing1991/markdown-preview.nvim",
    cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
    ft = "markdown",
    build = "cd app && npm install",
    init = function()
      vim.g.mkdp_filetypes = { "markdown" }
    end,
  },
}
