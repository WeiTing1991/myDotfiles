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
  -- -- better search
  -- {
  --   "MagicDuck/grug-far.nvim",
  --   lazy = true,
  --   enabled = false,
  --   opts = { headerMaxWidth = 80 },
  --   event = "VeryLazy",
  --   cmd = "GrugFar",
  --   keys = {
  --     {
  --       "<leader>/",
  --       function()
  --         local grug = require("grug-far")
  --         local ext = vim.bo.buftype == "" and vim.fn.expand("%:e")
  --         grug.open({
  --           transient = true,
  --           prefills = {
  --             filesFilter = ext and ext ~= "" and "*." .. ext or nil,
  --           },
  --         })
  --       end,
  --       mode = { "n", "v" },
  --       desc = "Search and Replace",
  --     },
  --   },
  -- },

  -- {
  --   "MeanderingProgrammer/render-markdown.nvim",
  --   enabled = false,
  --   opts = {
  --     code = {
  --       sign = false,
  --       width = "full",
  --       right_pad = 2,
  --       left_pad = 2,
  --       style = "none",
  --       disable_background = { "block" },
  --       border = "thick",
  --     },
  --     heading = {
  --       sign = false,
  --       icons = {},
  --     },
  --     checkbox = {
  --       enabled = false,
  --     },
  --
  --     completions = { blink = { enabled = true } },
  --   },
  --   ft = { "markdown", "norg", "rmd", "org", "codecompanion" },
  --   config = function(_, opts)
  --     require("render-markdown").setup(opts)
  --   end,
  -- },

  -- TODO highlight
  -- {
  --   "folke/todo-comments.nvim",
  --   enaled = true,
  --   lazy = true,
  --   event = "VeryLazy",
  --   config = function()
  --     require("todo-comments").setup({
  --       signs = true,
  --       sign_priority = 8,
  --       keywords = {
  --         FIX = {
  --           icon = " ", -- icon used for the sign, and in search results
  --           color = "error", -- can be a hex color, or a named color (see below)
  --           alt = { "FIXME", "BUG", "FIXIT", "ISSUE" }, -- a set of other keywords that all map to this FIX keywords
  --         },
  --         TODO = { icon = " ", color = "info" },
  --         HACK = { icon = " ", color = "warning" },
  --         WARN = { icon = " ", color = "warning", alt = { "WARNING", "XXX" } },
  --         CHECK = { icon = " ", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
  --         NOTE = { icon = " ", color = "hint", alt = { "INFO" } },
  --         TEST = { icon = "⏲ ", color = "test", alt = { "TESTING", "PASSED", "FAILED" } },
  --       },
  --       gui_style = {
  --         fg = "NONE", -- The gui style to use for the fg highlight group.
  --         bg = "BOLD", -- The gui style to use for the bg highlight group.
  --       },
  --       merge_keywords = true, -- when true, custom keywords will be merged with the defaults
  --       highlight = {
  --         multiline = true, -- enable multine todo comments
  --         multiline_pattern = "^.", -- lua pattern to match the next multiline from the start of the matched keyword
  --         multiline_context = 10, -- extra lines that will be re-evaluated when changing a line
  --         before = "", -- "fg" or "bg" or empty
  --         keyword = "wide", -- "fg", "bg", "wide", "wide_bg", "wide_fg" or empty. (wide and wide_bg is the same as bg, but will also highlight surrounding characters, wide_fg acts accordingly but with fg)
  --         after = "fg", -- "fg" or "bg" or empty
  --         pattern = [[.*<(KEYWORDS)\s*:]], -- pattern or table of patterns, used for highlighting (vim regex)
  --         comments_only = true, -- uses treesitter to match keywords in comments only
  --         max_line_len = 400, -- ignore lines longer than this
  --         exclude = {}, -- list of file types to exclude highlighting
  --       },
  --       colors = {
  --         error = { "DiagnosticError", "ErrorMsg", "#DC2626" },
  --         warning = { "DiagnosticWarn", "WarningMsg", "#FBBF24" },
  --         info = { "DiagnosticInfo", "#2563EB" },
  --         hint = { "DiagnosticHint", "#10B981" },
  --         default = { "Identifier", "#7C3AED" },
  --         test = { "Identifier", "#FF00FF" },
  --       },
  --       search = {
  --         command = "rg",
  --         args = {
  --           "--color=never",
  --           "--no-heading",
  --           "--with-filename",
  --           "--line-number",
  --           "--column",
  --         },
  --         pattern = [[\b(KEYWORDS):]], -- ripgrep regex
  --         -- pattern = [[\b(KEYWORDS)\b]], -- match without the extra colon. You'll likely get false positives
  --       },
  --     })
  --   end,
  -- },

  -- {
  --   "nvim-orgmode/orgmode",
  --   lazy = true,
  --   event = "VeryLazy",
  --   enabled = false,
  --   config = function()
  --     -- Setup orgmode
  --     require("orgmode").setup {
  --       org_agenda_files = "~/orgfiles/**/*",
  --       org_default_notes_file = "~/orgfiles/refile.org",
  --     }
  --   end,
  -- },
}
