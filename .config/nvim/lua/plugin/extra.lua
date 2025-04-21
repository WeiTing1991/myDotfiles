return {
  {
    'nvim-orgmode/orgmode',
    lazy = true,
    event = 'VeryLazy',
    enabled = false,
    config = function()
      -- Setup orgmode
      require('orgmode').setup({
        org_agenda_files = '~/orgfiles/**/*',
        org_default_notes_file = '~/orgfiles/refile.org',
      })
    end,
  },
  -- TODO highlight
  {
    "folke/todo-comments.nvim",
    lazy = true,
    -- enabled = false,
    event = "BufRead",
    -- config = function()
      -- require("todo-comments").setup {
      --   signs = false, -- show icons in the signs column
      --   sign_priority = 8, -- sign priority
      --   -- keywords recognized as todo comments
      --   keywords = {
      --     FIX = {
      --       icon = " ", -- icon used for the sign, and in search results
      --       color = "error", -- can be a hex color, or a named color (see below)
      --       alt = { "FIXME", "BUG", "FIXIT", "ISSUE" }, -- a set of other keywords that all map to this FIX keywords
      --       -- signs = false, -- configure signs for some keywords individually
      --     },
      --     TODO = { icon = " ", color = "info" },
      --     HACK = { icon = " ", color = "warning" },
      --     WARN = { icon = " ", color = "warning", alt = { "WARNING", "XXX" } },
      --     CHECK = { icon = " ", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
      --     NOTE = { icon = " ", color = "hint", alt = { "INFO" } },
      --     TEST = { icon = "⏲ ", color = "test", alt = { "TESTING", "PASSED", "FAILED" } },
      --   },
      --   gui_style = {
      --     fg = "NONE", -- The gui style to use for the fg highlight group.
      --     bg = "BOLD", -- The gui style to use for the bg highlight group.
      --   },
      --   merge_keywords = true, -- when true, custom keywords will be merged with the defaults
      --   -- highlighting of the line containing the todo comment
      --   -- * before: highlights before the keyword (typically comment characters)
      --   -- * keyword: highlights of the keyword
      --   -- * after: highlights after the keyword (todo text)
      --   highlight = {
      --     multiline = true, -- enable multine todo comments
      --     multiline_pattern = "^.", -- lua pattern to match the next multiline from the start of the matched keyword
      --     multiline_context = 10, -- extra lines that will be re-evaluated when changing a line
      --     before = "", -- "fg" or "bg" or empty
      --     keyword = "wide", -- "fg", "bg", "wide", "wide_bg", "wide_fg" or empty. (wide and wide_bg is the same as bg, but will also highlight surrounding characters, wide_fg acts accordingly but with fg)
      --     after = "fg", -- "fg" or "bg" or empty
      --     pattern = [[.*<(KEYWORDS)\s*:]], -- pattern or table of patterns, used for highlighting (vim regex)
      --     comments_only = true, -- uses treesitter to match keywords in comments only
      --     max_line_len = 400, -- ignore lines longer than this
      --     exclude = {}, -- list of file types to exclude highlighting
      --   },
      --   -- list of named colors where we try to extract the guifg from the
      --   -- list of highlight groups or use the hex color if hl not found as a fallback
      --   colors = {
      --     error = { "DiagnosticError", "ErrorMsg", "#DC2626" },
      --     warning = { "DiagnosticWarn", "WarningMsg", "#FBBF24" },
      --     info = { "DiagnosticInfo", "#2563EB" },
      --     hint = { "DiagnosticHint", "#10B981" },
      --     default = { "Identifier", "#7C3AED" },
      --     test = { "Identifier", "#FF00FF" },
      --   },
      --   search = {
      --     command = "rg",
      --     args = {
      --       "--color=never",
      --       "--no-heading",
      --       "--with-filename",
      --       "--line-number",
      --       "--column",
      --     },
      --     -- regex that will be used to match keywords.
      --     -- don't replace the (KEYWORDS) placeholder
      --     pattern = [[\b(KEYWORDS):]], -- ripgrep regex
      --     -- pattern = [[\b(KEYWORDS)\b]], -- match without the extra colon. You'll likely get false positives
      --   },
      -- }
    -- end,
  },

  {
    "yetone/avante.nvim",
    event = "VeryLazy",
    lazy = false,
    enabled = false,
    version = false, -- Set this to "*" to always pull the latest release version, or set it to false to update to the latest code changes.
    opts = {
      provider = "copilot",
      copilot = {
        endpoint = "https://api.githubcopilot.com",
        model = "claude-3.7-sonnet",
        proxy = nil, -- [protocol://]host[:port] Use this proxy
        allow_insecure = false, -- Allow insecure server connections
        timeout = 30000, -- Timeout in milliseconds
        temperature = 0,
        max_tokens = 4096,
      },
    },
    -- if you want to build from source then do `make BUILD_FROM_SOURCE=true`
    build = "make",
    -- build = "powershell -ExecutionPolicy Bypass -File Build.ps1 -BuildFromSource false" -- for windows
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "stevearc/dressing.nvim",
      "MunifTanjim/nui.nvim",
      {
        -- support for image pasting
        "HakonHarnes/img-clip.nvim",
        event = "VeryLazy",
        opts = {
          -- recommended settings
          default = {
            embed_image_as_base64 = false,
            prompt_for_file_name = false,
            drag_and_drop = {
              insert_mode = true,
            },
            -- required for Windows users
            use_absolute_path = true,
          },
        },
      },
      {
        "MeanderingProgrammer/render-markdown.nvim",
        opts = {
          file_types = { "Avante" },
        },
        ft = { "Avante" },
      },
    },
  },
}
