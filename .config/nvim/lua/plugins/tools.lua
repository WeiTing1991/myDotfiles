return {
  -- tmux navigator
  {
    "christoomey/vim-tmux-navigator",
    lazy = true,
    event = "BufEnter",
    enabled = false,
    cmd = {
      "TmuxNavigateLeft",
      "TmuxNavigateDown",
      "TmuxNavigateUp",
      "TmuxNavigateRight",
      "TmuxNavigatePrevious",
    },
    keys = {
      { "<c-h>", "<cmd><C-U>TmuxNavigateLeft<cr>" },
      { "<c-j>", "<cmd><C-U>TmuxNavigateDown<cr>" },
      { "<c-k>", "<cmd><C-U>TmuxNavigateUp<cr>" },
      { "<c-l>", "<cmd><C-U>TmuxNavigateRight<cr>" },
      { "<c-\\>", "<cmd><C-U>TmuxNavigatePrevious<cr>" },
    },
  },

  -- diagnostics
  {
    "folke/trouble.nvim",
    lazy = true,
    event = "VeryLazy",
    cmd = "Trouble",
    keys = {
      ["j"] = "next",
      ["k"] = "prev",
    },
    opts = {
      focus = false,
      auto_preview = true,
      preview = {
        type = "float",
        relative = "editor",
        size = { width = 0.8, height = 0.3 }, -- Smaller preview
        position = { 0.5, 0.8 },
        border = "single",
      },
    },
  },
  -- this is really cool
  {
    "lucaSartore/fastspell.nvim",
    lazy = true,
    event = "VeryLazy",
    build = function()
      local base_path = vim.fn.stdpath("data") .. "/lazy/fastspell.nvim"
      local extension = vim.fn.has("win32") == 1 and "cmd" or "sh"
      local cmd = base_path .. "/lua/scripts/install." .. extension
      vim.system({ cmd })
    end,
    config = function()
      local fastspell = require("fastspell")

      fastspell.setup({
        diagnostic_severity = vim.diagnostic.severity.HINT,
        -- cspell_json_file_path = vim.fn.stdpath("config") .. "/cspell.json"
      })

      vim.api.nvim_create_autocmd({ "TextChanged", "TextChangedI", "BufEnter", "WinScrolled" }, {
        callback = function(_)
          if not vim.g.spell_enabled then
            return
          end
          local first_line = vim.fn.line("w0") - 1
          local last_line = vim.fn.line("w$")
          fastspell.sendSpellCheckRequest(first_line, last_line)
        end,
      })
    end,
  },

  -- Markdown
  {
    "WeiTing1991/markdown-preview.nvim",
    --NOTE: Missing dependencies npm install -g tslib
    cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
    build = "cd app && npm install",
    ft = { "markdown" },
    init = function()
      vim.g.mkdp_filetypes = { "markdown" }
    end,
  },
}
