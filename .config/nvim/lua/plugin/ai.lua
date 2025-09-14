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
        -- copilot_model = "gpt-4o-copilot",
        server_opts_overrides = {
          trace = "verbose",
          settings = {
            advanced = {
              -- completions for panel
              listCount = 15,
              -- completions for getCompletions
              inlineSuggestCount = 3,
            },
          },
        },
        workspaces_folder = {
          "~/project/",
          "~/work/",
        },
        vim.api.nvim_set_hl(0, "CopilotSuggestion", {
          fg = "#676767",
          italic = true,
        }),
      })
    end,
  },
  -- check open code
  -- run `claude install` for local build
  {
    "coder/claudecode.nvim",
    dependencies = { "folke/snacks.nvim" },
    lazy = true,
    event = "VeryLazy",
    config = true,
    opts = {
      terminal = {
        split_width_percentage = 0.25,
        split_side = "left",
      },
    },
    keys = {
      { "<leader>a", nil, desc = "AI/Claude Code" },
      { "<leader>ac", "<cmd>ClaudeCode<cr>", desc = "Toggle Claude" },
      { "<leader>af", "<cmd>ClaudeCodeFocus<cr>", desc = "Focus Claude" },
      { "<leader>ar", "<cmd>ClaudeCode --resume<cr>", desc = "Resume Claude" },
      { "<leader>aC", "<cmd>ClaudeCode --continue<cr>", desc = "Continue Claude" },
      { "<leader>am", "<cmd>ClaudeCodeSelectModel<cr>", desc = "Select Claude model" },
      { "<leader>ab", "<cmd>ClaudeCodeAdd %<cr>", desc = "Add current buffer" },
      { "<leader>as", "<cmd>ClaudeCodeSend<cr>", mode = "v", desc = "Send to Claude" },
      -- {
      --   "<leader>as",
      --   "<cmd>ClaudeCodeTreeAdd<cr>",
      --   desc = "Add file",
      --   ft = { "NvimTree", "neo-tree", "oil", "minifiles" },
      -- },
      -- Diff management
      { "<leader>aa", "<cmd>ClaudeCodeDiffAccept<cr>", desc = "Accept diff" },
      { "<leader>ad", "<cmd>ClaudeCodeDiffDeny<cr>", desc = "Deny diff" },
    },
  },
}
