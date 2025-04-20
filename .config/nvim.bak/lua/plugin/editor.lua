return {
  --[[ EDITOR ]]
  -- todo highlight
  {
    "folke/todo-comments.nvim",
    lazy = true,
    event = "BufRead",
    config = function()
      require "configs.todo"
    end,
  },

  -- indentscope
  {
    "echasnovski/mini.indentscope",
    version = false,
    lazy = true,
    event = "BufRead",
    config = function()
      require "configs.indentscope"
    end,
  },


  {
    "MagicDuck/grug-far.nvim",
    lazy = true,
    opts = { headerMaxWidth = 80 },
    -- event = "BufRead",
    cmd = "GrugFar",
    keys = {
      {
        "<leader>/",
        function()
          local grug = require "grug-far"
          local ext = vim.bo.buftype == "" and vim.fn.expand "%:e"
          grug.open {
            transient = true,
            prefills = {
              filesFilter = ext and ext ~= "" and "*." .. ext or nil,
            },
          }
        end,
        mode = { "n", "v" },
        desc = "Search and Replace",
      },
    },
  },

  -- NOTE: https://github.com/echasnovski/mini.nvim/blob/main/readmes/mini-ai.md

  -- CHECK:
  {
    "echasnovski/mini.align",
    lazy = true,
    version = "*",
    event = "InsertEnter",
    config = function()
      require("mini.align").setup()
    end,
  },


