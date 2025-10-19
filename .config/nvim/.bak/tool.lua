return {
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
  -- column
  {
    "luukvbaal/statuscol.nvim",
    lazy = true,
    event = "BufEnter",
    config = function()
      local builtin = require("statuscol.builtin")
      require("statuscol").setup({
        setopt = true,
        ft_ignore = { "snacks_dashboard", "help", "lazy", "mason", "NvimTree", "undotree" },
        segments = {
          { text = { "%s" }, click = "v:lua.ScFa", maxwidth = 2 },

          { text = { builtin.lnumfunc }, click = "v:lua.scla" },
          {
            text = { " ", builtin.foldfunc, " " },
            condition = { builtin.not_empty, true, builtin.not_empty },
            click = "v:lua.scfa",
          },
        },
      })
    end,
  },

  -- BETTER fold
  -- TODO: make a PR
  {
    "kevinhwang91/nvim-ufo",
    lazy = true,
    event = "VeryLazy",
    dependencies = {
      "kevinhwang91/promise-async",
    },
    opts = {
      open_fold_hl_timeout = 0,
    },
    config = function(_, opts)
      vim.api.nvim_create_autocmd("FileType", {
        pattern = "*",
        callback = function()
          if vim.bo.filetype == "snacks_dashboard" then
            vim.wo.foldcolumn = "0" -- Use window-local setting
          else
            vim.wo.foldcolumn = "1"
          end
        end,
      })

      vim.opt.fillchars:append({
        foldopen = "",
        foldsep = " ",
        foldclose = "",
      })

      local capabilities = vim.lsp.protocol.make_client_capabilities()
      capabilities.textDocument.foldingRange = {
        dynamicRegistration = false,
        lineFoldingOnly = true,
      }
      local language_servers = vim.lsp.get_clients() -- or list servers manually like {'gopls', 'clangd'}
      for _, ls in ipairs(language_servers) do
        require("lspconfig")[ls].setup({
          capabilities = capabilities,
        })
      end
      local handler = function(virtText, lnum, endLnum, width, truncate)
        local newVirtText = {}
        local suffix = (" 󰁂 %d "):format(endLnum - lnum)
        local sufWidth = vim.fn.strdisplaywidth(suffix)
        local targetWidth = width - sufWidth
        local curWidth = 0
        for _, chunk in ipairs(virtText) do
          local chunkText = chunk[1]
          local chunkWidth = vim.fn.strdisplaywidth(chunkText)
          if targetWidth > curWidth + chunkWidth then
            table.insert(newVirtText, chunk)
          else
            chunkText = truncate(chunkText, targetWidth - curWidth)
            local hlGroup = chunk[2]
            table.insert(newVirtText, { chunkText, hlGroup })
            chunkWidth = vim.fn.strdisplaywidth(chunkText)
            -- str width returned from truncate() may less than 2nd argument, need padding
            if curWidth + chunkWidth < targetWidth then
              suffix = suffix .. (" "):rep(targetWidth - curWidth - chunkWidth)
            end
            break
          end
          curWidth = curWidth + chunkWidth
        end
        table.insert(newVirtText, { suffix, "MoreMsg" })
        return newVirtText
      end
      opts.fold_virt_text_handler = handler
      require("ufo").setup(opts)
    end,
  },
  {
    "catgoose/nvim-colorizer.lua",
    event = "BufReadPre",
    lazy = true,
    opts = {},
    config = function()
      require("colorizer").setup({
        filetypes = {
          "*",
          "!vim",
          "!mason",
          "!lazy",
        },
        user_default_options = {
          names = false,
        },
      })
    end,
  },
}
