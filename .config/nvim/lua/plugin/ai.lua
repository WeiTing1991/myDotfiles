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
          debounce = 75,
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
        copilot_model = "gpt-4o",
        -- copilot_model = "claude-3.7-sonnet",
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
  {
    "WeiTing1991/codecompanion.nvim",
    lazy = true,
    event = "VeryLazy",
    -- enabled = false,
    opts = {},
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter",
    },
    config = function()
      require("codecompanion").setup({
        strategies = {
          chat = { adapter = "copilot" },
          inline = { adapter = "copilot" },
        },
        opts = {
          log_level = "DEBUG",
        },
      })
      -- keymaps
      vim.keymap.set({ "n", "v" }, "<M-i>i", function()
        require("core.ui_select").with_custom_select(function()
          vim.cmd("CodeCompanionActions")
        end)
      end, { desc = "CodeCompanionActions" })

      vim.keymap.set({ "n", "v" }, "<M-i>t", function()
        vim.cmd("CodeCompanionChat Toggle")
      end, { desc = "CodeCompanionChat Toggle" })
    end,
  },

  -- {
  --   "yetone/avante.nvim",
  --   event = "VeryLazy",
  --   lazy = false,
  --   enabled = false,
  --   version = "*",
  --   opts = {
  --     provider = "copilot",
  --     copilot = {
  --       endpoint = "https://api.githubcopilot.com",
  --       model = "claude-3.7-sonnet",
  --       proxy = nil, -- [protocol://]host[:port] Use this proxy
  --       allow_insecure = false, -- Allow insecure server connections
  --       timeout = 30000, -- Timeout in milliseconds
  --       temperature = 0,
  --       max_tokens = 4096,
  --     },
  --   },
  --   -- if you want to build from source then do `make BUILD_FROM_SOURCE=true`
  --   build = "make",
  --   -- build = "powershell -ExecutionPolicy Bypass -File Build.ps1 -BuildFromSource false" -- for windows
  --   dependencies = {
  --     "nvim-treesitter/nvim-treesitter",
  --     "stevearc/dressing.nvim",
  --     "MunifTanjim/nui.nvim",
  --     {
  --       -- support for image pasting
  --       "HakonHarnes/img-clip.nvim",
  --       event = "VeryLazy",
  --       opts = {
  --         -- recommended settings
  --         default = {
  --           embed_image_as_base64 = false,
  --           prompt_for_file_name = false,
  --           drag_and_drop = {
  --             insert_mode = true,
  --           },
  --           -- required for Windows users
  --           use_absolute_path = true,
  --         },
  --       },
  --     },
  --     {
  --       "MeanderingProgrammer/render-markdown.nvim",
  --       opts = {
  --         file_types = { "Avante" },
  --       },
  --       ft = { "Avante" },
  --     },
  --   },
  -- },
}
