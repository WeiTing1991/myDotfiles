return {
  -- Copilot
  {
    "zbirenbaum/copilot.lua",
    cmd = "Copilot",
    event = "InsertEnter",
    config = function()
      require("copilot").setup({
        panel = { enabled = false },
        suggestion = {
          enabled = true,
          auto_trigger = true,
          hide_during_completion = true,
          debounce = 50,
          keymap = {
            accept = false,
            accept_word = "<A-f>",
            accept_line = vim.fn.has("mac") == 1 and "<M-l>" or "<A-l>",
            dismiss = "<Esc>",
          },
        },
        filetypes = { ["."] = true },
        server_opts_overrides = {
          trace = "verbose",
          settings = {
            advanced = {
              listCount = 10,
              inlineSuggestCount = 3,
            },
          },
        },
      })
    end,
    keys = {
      {
        "<leader>tc",
        function()
          require("copilot.suggestion").toggle_auto_trigger()
          if not vim.b.copilot_suggestion_auto_trigger then
            print("Copilot is disabled")
          else
            print("Copilot is enabled")
          end
        end,
        desc = "Toggle Copilot",
      },
    },
  },

  -- OpenCode
  {
    "nickjvandyke/opencode.nvim",
    version = "*",
    dependencies = {
      {
        "WeiTing1991/snacks.nvim",
        optional = true,
        opts = {
          input = {},
          picker = {
            actions = {
              opencode_send = function(...) return require("opencode").snacks_picker_send(...) end,
            },
            win = {
              input = {
                keys = { ["<a-a>"] = { "opencode_send", mode = { "n", "i" } } },
              },
            },
          },
          terminal = {},
        },
      },
    },
    config = function()
      ---@type opencode.Opts
      vim.g.opencode_opts = {
        provider = {
          enabled = "snacks",
          snacks = {
            win = {
              position = "left",
              width = 0.35,
            },
          },
        },
      }

      vim.o.autoread = true
    end,
    keys = {
      { "<C-a>", function() require("opencode").ask("@this: ", { submit = true }) end, mode = { "n", "x" }, desc = "Ask opencode" },
      { "<C-x>", function() require("opencode").select() end, mode = { "n", "x" }, desc = "Execute opencode action" },
      { "<C-.>", function() require("opencode").toggle() end, mode = { "n", "t" }, desc = "Toggle opencode" },
      { "go", function() return require("opencode").operator("@this ") end, mode = { "n", "x" }, desc = "Add range to opencode", expr = true },
      { "goo", function() return require("opencode").operator("@this ") .. "_" end, desc = "Add line to opencode", expr = true },
    },
  },
}
