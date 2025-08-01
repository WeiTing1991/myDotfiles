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
          debounce = 30,
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
        vim.api.nvim_set_hl(0, "CopilotSuggestion", {
          -- fg = "#676767",
          -- fg = "#8a8a8a",
          ctermfg = 8,
          italic = true,
        }),
      })
    end,
  },
  {
    "WeiTing1991/codecompanion.nvim",
    lazy = true,
    event = "VeryLazy",
    opts = {},
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter",
    },
    config = function()
      vim.opt.winwidth = 10
      require("codecompanion").setup({
        strategies = {
          chat = {
            adapter = "copilot",
            window = {
              layout = "vertical", -- float|vertical|horizontal|buffer
              position = "left",   -- locked by vim.opt.splitright
              border = "single",
              height = 0.8,
              width = 0.25,
              relative = "editor",
              full_height = true, -- when set to false, vsplit will be used to open the chat buffer vs. botright/topleft vsplit
              sticky = true,      -- when set to true and `layout` is not `"buffer"`, the chat buffer will remain opened when switching tabs
              opts = {
                breakindent = true,
                cursorcolumn = false,
                cursorline = false,
                foldcolumn = "0",
                linebreak = true,
                list = false,
                numberwidth = 0,
                signcolumn = "no",
                spell = false,
                wrap = true,
              },
            },
          },
          inline = { adapter = "copilot" },
        },
        adapters = {
          copilot = function()
            return require("codecompanion.adapters").extend("copilot", {
              schema = {
                model = {
                  default = "claude-3.7-sonnet",
                  choices = {
                    ["o3-mini-2025-01-31"] = { opts = { can_reason = true } },
                    ["o1-2024-12-17"] = { opts = { can_reason = true } },
                    ["o1-mini-2024-09-12"] = { opts = { can_reason = true } },
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
