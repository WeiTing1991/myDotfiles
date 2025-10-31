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
          debounce = 50,
          keymap = {
            accept = false,
            accept_word = "<A-f>",
            accept_line = vim.fn.has("mac") == 1 and "<M-l>" or "<A-l>",
            -- next = "<C-]>",
            -- prev = "<C-[>",
            dismiss = "<Esc>",
            -- dismiss = "<C-c>",
          },
        },
        filetypes = {
          -- yaml = false,
          -- markdown = true,
          -- help = false,
          -- gitcommit = false,
          -- gitrebase = false,
          -- hgcommit = false,
          -- svn = false,
          -- cvs = false,
          ["."] = true,
        },
        server_opts_overrides = {
          trace = "verbose",
          settings = {
            advanced = {
              listCount = 10,
              inlineSuggestCount = 3,
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
    "folke/sidekick.nvim",
    lazy = true,
    event = "VeryLazy",
    opts = {
      nes = {
        enabled = false,
        debounce = 50,
      },
      signs = {
        enabled = true,
        icon = " ",
      },
      cli = {
        win = {
          layout = "left",
          split = {
            width = 50,
          },
        },
      },
    },
    keys = {
      -- {
      --   "<leader>aa",
      --   function()
      --     require("sidekick.cli").toggle({ focus = true })
      --   end,
      --   desc = "Toggle AI CLI",
      --   mode = { "n", "v" },
      -- },
      {
        "<A-i>",
        function()
          require("sidekick.cli").prompt()
        end,
        desc = "Select AI Prompt",
        mode = { "n", "v" },
      },
    },
  },
}
