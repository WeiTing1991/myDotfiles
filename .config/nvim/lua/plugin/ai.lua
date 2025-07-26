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
}
