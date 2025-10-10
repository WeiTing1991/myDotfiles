return {
  {
    "WeiTing1991/gruvbox.nvim",
    enabled = false,
    priority = 1000,
    config = true,
  },
  {
    "Mofiqul/dracula.nvim",
    priority = 1000,
    enabled = true,
    config = function()
      vim.cmd([[colorscheme dracula]])
    end,
  },
  {
    "projekt0n/github-nvim-theme",
    name = "github-theme",
    priority = 1000,
    config = function()
      require("github-theme").setup({})
    end,
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
      require("plugin.configs.lualine")
    end,
  },

  {
    "nanozuki/tabby.nvim",
    lazy = false,
    ---@type TabbyConfig
    opts = {},
  },
}
