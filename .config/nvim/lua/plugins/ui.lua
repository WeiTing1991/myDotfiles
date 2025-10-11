return {
  {
    "WeiTing1991/gruvbox.nvim",
    priority = 1000,
    config = true,
    config = function()
      vim.cmd([[colorscheme gruvbox]])
    end,
  },
  {
    "Mofiqul/dracula.nvim",
    priority = 1000,
    -- config = function()
    --   vim.cmd([[colorscheme dracula]])
    -- end,
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
      require("plugins.configs.lualine")
    end,
  },
  {
    "nanozuki/tabby.nvim",
    event = "VeryLazy",
    lazy = true,
    opts = {},
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
