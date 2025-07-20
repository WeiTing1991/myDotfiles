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
              listCount = 10,         -- #completions for panel
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
  {
    "WeiTing1991/codecompanion.nvim",
    lazy = true,
    event = "VeryLazy",
    enabled = false,
    opts = {},
    config = function()
      require("codecompanion").setup({
        display = {
          chat = {
            window = {
              layout = "vertical",
              position = "left",
              width = 0.3,
              height = 0.9,
              border = "rounded",
              full_height = true,
            },
          },
        },
        strategies = {
          chat = { adapter = "copilot" },
          inline = { adapter = "copilot" },
        },
        adapters = {
          copilot = function()
            return require("codecompanion.adapters").extend("copilot", {
              schema = {
                model = {
                  default = "gemini-2.0-flash-001",
                  -- Supported models based on latest codecompanion documentation:
                  choices = {
                    "o3-mini-2025-01-31",
                    "o1-2024-12-17",
                    "o1-mini-2024-09-12",
                    "claude-3.5-sonnet",
                    "claude-3.7-sonnet",
                    "claude-3.7-sonnet-thought",
                    "gpt-4o-2024-08-06",
                    "gemini-2.0-flash-001",
                  },
                },
              },
            })
          end,
        },
        opts = {
          log_level = "DEBUG",
        },
      })
    end,
  },
}
