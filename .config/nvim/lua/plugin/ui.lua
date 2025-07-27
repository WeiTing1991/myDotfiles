return {
  {
    "projekt0n/github-nvim-theme",
    name = "github-theme",
    lazy = false,
    priority = 1000,
    config = function()
      require("github-theme").setup({})
      vim.cmd.colorscheme("github_dark")
    end,
  },
  {
    "nvim-lualine/lualine.nvim",
    event = "VeryLazy",
    lazy = true,
    init = function()
      vim.g.lualine_laststatus = vim.o.laststatus
      if vim.fn.argc(-1) > 0 then
        -- set an empty statusline till lualine loads
        vim.o.statusline = " "
      else
        -- hide the statusline on the starter page
        vim.o.laststatus = 0
      end
    end,
    config = function()
      require('lualine').setup()
    end,
  },
  {
    "nanozuki/tabby.nvim",
    ---@type TabbyConfig
    lazy = false,
    opts = {
    },
  },
  -- Statusline
  -- {
  --   "WeiTing1991/staline.nvim",
  --   priority = 1000,
  --   enabled =false,
  --   lazy = false,
  --   config = function()
  --     require("plugin.configs.staline")
  --   end,
  -- },
}
