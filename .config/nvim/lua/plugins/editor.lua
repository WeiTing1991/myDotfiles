return {
  -- Keymap helper popup
  {
    "echasnovski/mini.clue",
    version = "*",
    event = "VeryLazy",
    config = function()
      local miniclue = require("mini.clue")

      local function macro_clues()
        local res = {}
        for _, register in ipairs(vim.split("abcdefghijklmnopqrstuvwxyz", "")) do
          local keys = string.format('"%s', register)
          local ok, desc = pcall(vim.fn.getreg, register)
          if ok and desc ~= "" then
            ---@cast desc string
            desc = string.format("register: %s", desc:gsub("%s+", " "))
            table.insert(res, { mode = "n", keys = keys, desc = desc })
            table.insert(res, { mode = "v", keys = keys, desc = desc })
          end
        end
        return res
      end

      local function mark_clues()
        local marks = {}
        vim.list_extend(marks, vim.fn.getmarklist(vim.api.nvim_get_current_buf()))
        vim.list_extend(marks, vim.fn.getmarklist())

        return vim
          .iter(marks)
          :map(function(mark)
            local key = mark.mark:sub(2, 2)
            if not string.match(key, "^%a") then
              return nil
            end
            local desc
            if mark.file then
              desc = vim.fn.fnamemodify(mark.file, ":p:~:.")
            elseif mark.pos[1] and mark.pos[1] ~= 0 then
              local line_num = mark.pos[2]
              local lines = vim.fn.getbufline(mark.pos[1], line_num)
              if lines and lines[1] then
                desc = string.format("%d: %s", line_num, lines[1]:gsub("^%s*", ""))
              end
            end
            if desc then
              return { mode = "n", keys = string.format("`%s", key), desc = desc }
            end
          end)
          :totable()
      end

      miniclue.setup({
        triggers = {
          { mode = "n", keys = "<leader>" },
          { mode = "x", keys = "<leader>" },
          { mode = "n", keys = "g" },
          { mode = "x", keys = "g" },
          { mode = "n", keys = "'" },
          { mode = "n", keys = "`" },
          { mode = "x", keys = "'" },
          { mode = "x", keys = "`" },
          { mode = "n", keys = '"' },
          { mode = "x", keys = '"' },
          { mode = "i", keys = "<C-r>" },
          { mode = "c", keys = "<C-r>" },
          { mode = "n", keys = "<C-w>" },
          { mode = "n", keys = "z" },
          { mode = "x", keys = "z" },
        },
        clues = {
          miniclue.gen_clues.builtin_completion(),
          miniclue.gen_clues.marks(),
          miniclue.gen_clues.registers(),
          { mode = "n", keys = "<leader>", desc = "leader" },
          { mode = "n", keys = "<leader>f", desc = "+find" },
          { mode = "n", keys = "<leader>g", desc = "+git" },
          { mode = "n", keys = "<leader>t", desc = "+toggle/tab" },
          { mode = "n", keys = "<leader>x", desc = "+diagnostics" },
          { mode = "n", keys = "<leader>r", desc = "+refactor" },
          { mode = "x", keys = "<leader>r", desc = "+refactor" },
          mark_clues,
          macro_clues,
        },
        window = {
          delay = 300,
          scroll_down = "<C-d>",
          scroll_up = "<C-u>",
          config = function(bufnr)
            local max_width = 0
            for _, line in ipairs(vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)) do
              max_width = math.max(max_width, vim.fn.strchars(line))
            end
            max_width = max_width + 2
            return { width = math.min(70, max_width) }
          end,
        },
      })
    end,
  },

  -- Real-time spell checker
  {
    "lucaSartore/fastspell.nvim",
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

  -- Auto detect indentation
  {
    "tpope/vim-sleuth",
    event = "BufReadPost",
  },

  -- Surround (cs'" ; ysiw))
  {
    "kylechui/nvim-surround",
    event = "BufReadPost",
    config = function()
      require("nvim-surround").setup({})
    end,
  },

  -- Better select with a and i
  {
    "echasnovski/mini.ai",
    event = "BufReadPost",
    version = "*",
    config = function()
      require("mini.ai").setup()
    end,
  },

  -- Autoclosing braces
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    config = function()
      require("nvim-autopairs").setup()
    end,
  },

  -- Comment
  {
    "numToStr/Comment.nvim",
    event = "BufReadPost",
    opts = {},
  },
}
