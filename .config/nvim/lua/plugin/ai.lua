return {
  -- Copilot
  {
    "zbirenbaum/copilot.lua",
    lazy = true,
    cmd = "Copilot",
    event = "InsertEnter",
    config = function()
      require("copilot").setup({
        panel = {
          enabled = false,
        },
        suggestion = {
          enabled = true,
          auto_trigger = true,
          hide_during_completion = true,
          debounce = 100,
          keymap = {
            accept = false,
            accept_word = "<C-f>",
            accept_line = "<C-l>",
            next = "<C-]>",
            prev = "<C-[>",
            dismiss = "<Esc>",
            -- dismiss = "<C-c>",
          },
        },
        filetypes = {
          yaml = false,
          markdown = true,
          help = false,
          gitcommit = false,
          gitrebase = false,
          hgcommit = false,
          svn = false,
          cvs = false,
          ["."] = false,
        },
        copilot_node_command = "node",
        -- copilot_model = "",
        copilot_model = "gpt-4o-copilot",
        server_opts_overrides = {
          trace = "verbose",
          settings = {
            advanced = {
              listCount = 10, -- #completions for panel
              inlineSuggestCount = 3, -- #completions for getCompletions
            },
          },
        },
        workspaces_folder = {
          "~/project/",
          "~/work/",
        },
      })
    end,
  },
  -- {
  --   "WeiTing1991/codecompanion.nvim",
  --   lazy = true,
  --   event = "VeryLazy",
  --   -- enabled = false,
  --   opts = {},
  --   dependencies = {
  --     "nvim-lua/plenary.nvim",
  --     "nvim-treesitter/nvim-treesitter",
  --   },
  --   config = function()
  --     require("codecompanion").setup({
  --       strategies = {
  --         chat = { adapter = "copilot" },
  --         inline = { adapter = "copilot" },
  --       },
  --       adapters = {
  --         copilot = function()
  --           return require("codecompanion.adapters").extend("copilot", {
  --             schema = {
  --               model = {
  --                 default = "claude-3.7-sonnet",
  --                 choices = {
  --                   "gpt-4o-2024-05-13",
  --                   "gpt-4o-2024-08-06",
  --                   "claude-3.5-sonnet",
  --                   "claude-3.7-sonnet",
  --                   "claude-4-sonnet",
  --                   "o1-preview-2024-09-12",
  --                   "o1-mini-2024-09-12",
  --                 },
  --               },
  --             },
  --           })
  --         end,
  --       },
  --       opts = {
  --         log_level = "DEBUG",
  --       },
  --     })
  --     -- keymaps
  --     vim.keymap.set({ "n", "v" }, "<M-i>i", function()
  --       require("core.ui_select").with_custom_select(function()
  --         vim.cmd("CodeCompanionActions")
  --       end)
  --     end, { desc = "CodeCompanionActions" })

  --     vim.keymap.set({ "n", "v" }, "<M-i>t", function()
  --       vim.cmd("CodeCompanionChat Toggle")
  --     end, { desc = "CodeCompanionChat Toggle" })
  --   end,
  -- },
}
