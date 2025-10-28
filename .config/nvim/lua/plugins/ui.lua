return {
  {
    "WeiTing1991/gruvbox.nvim",
    priority = 1000,
    lazy = true,
  },
  {
    "Mofiqul/dracula.nvim",
    priority = 1000,
    lazy = true,
  },
  {
    "projekt0n/github-nvim-theme",
    priority = 1000,
    lazy = true,
  },
  {
    "nvim-lualine/lualine.nvim",
    event = "VeryLazy",
    lazy = true,
    init = function()
      vim.g.lualine_laststatus = vim.o.laststatus
      if vim.fn.argc(-1) > 0 then
        vim.o.statusline = " "
      else
        vim.o.laststatus = 0
      end
    end,
    config = function()
      require("plugins.configs.lualine")
    end,
  },
  {
    "nanozuki/tabby.nvim",
    lazy = true,
    enabled = false,
    event = "VeryLazy",
    opts = {},
    -- config = function()
    --   local theme = {
    --     fill = "TabLineFill",
    --     head = "TabLine",
    --     current_tab = "TabLineSel",
    --     tab = "TabLine",
    --     win = "TabLine",
    --     tail = "TabLine",
    --   }
    --   require("tabby").setup({
    --     line = function(line)
    --       return {
    --         {
    --           { "  ", hl = theme.head },
    --           line.sep("|", theme.head, theme.fill),
    --         },
    --         line.tabs().foreach(function(tab)
    --           local hl = tab.is_current() and theme.current_tab or theme.tab
    --           return {
    --             line.sep("", hl, theme.fill),
    --             tab.is_current() and "" or "󰆣",
    --             tab.number(),
    --             tab.name(),
    --             tab.close_btn(""),
    --             line.sep("", hl, theme.fill),
    --             hl = hl,
    --             margin = " ",
    --           }
    --         end),
    --         line.spacer(),
    --         line.wins_in_tab(line.api.get_current_tab()).foreach(function(win)
    --           return {
    --             line.sep("", theme.win, theme.fill),
    --             win.is_current() and "" or "",
    --             win.buf_name(),
    --             line.sep("", theme.win, theme.fill),
    --             hl = theme.win,
    --             margin = " ",
    --           }
    --         end),
    --         {
    --           line.sep("", theme.tail, theme.fill),
    --           { "  ", hl = theme.tail },
    --         },
    --         hl = theme.fill,
    --       }
    --     end,
    --   })
    -- end,
  },
  {
    "lukas-reineke/virt-column.nvim",
    lazy = true,
    event = "BufRead",
    opts = {
      char = { "┆" },
      virtcolumn = "120",
      highlight = { "NonText" },
      exclude = { filetypes = { "oil", "markdown" } },
    },
  },
}
