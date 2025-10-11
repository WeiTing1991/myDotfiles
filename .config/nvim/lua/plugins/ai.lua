return {
  -- Copilot
  {
    "zbirenbaum/copilot.lua",
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
          keymap = {
            accept = "<M-l>",
            next = "<M-]>",
            prev = "<M-[>",
            dismiss = "<C-]>",
          },
        },
      })
    end,
  },
  -- https://github.com/folke/sidekick.nvim
  {
    "folke/sidekick.nvim",
    event = "BufEnter",
    dependencies = { "zbirenbaum/copilot.lua" },
    opts = {
      nes = {
        enabled = true,
        debounce = 100,
      },
      cli = {
        tools = {
          claude = { cmd = { "claude" } },
        },
      },
    },
    keys = {
      {
        "<leader>aa",
        function()
          require("sidekick.cli").toggle({ focus = true })
        end,
        desc = "Toggle AI CLI",
        mode = { "n", "v" },
      },
      {
        "<leader>ac",
        function()
          require("sidekick.cli").toggle({ name = "claude", focus = true })
        end,
        desc = "Open Claude",
        mode = { "n", "v" },
      },
      {
        "<leader>ap",
        function()
          require("sidekick.cli").prompt()
        end,
        desc = "Select AI Prompt",
        mode = { "n", "v" },
      },
    },
  },
}
