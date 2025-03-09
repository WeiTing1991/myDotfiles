-- NOTE:

-- https://github.com/CopilotC-Nvim/CopilotChat.nvim
-- https://github.com/folke/edgy.nvim?tab=readme-ov-file
-- list to be check further.
-- "CopilotC-Nvim/CopilotChat.nvim",
-- https://github.com/ThePrimeagen/refactoring.nvim
-- https://github.com/tpope/vim-fugitive
-- https://github.com/sontungexpt/sttusline/tree/table_version

-- CHECK: https://github.com/folke/snacks.nvim/tree/main?tab=readme-ov-file
--
-- {
--   "stevearc/dressing.nvim",
--   enabled = false,
--   event = "VeryLazy",
--   opts = {},
-- },
--
return {
  {
    "folke/snacks.nvim",
    priority = 1000,
    lazy = false,
    ---@type snacks.Config
    opts = {
      animate = { enabled = false },
      bigfile = { enabled = false },
      bufdelete = {},
      dashboard = { enabled = false },
      debug = { enabled = false },
      dim = { enabled = false },
      explorer = { enabled = false },
      git = { enabled = false },
      gitbrowse = {},
      image = { enabled = false },
      indent = { enabled = false },
      input = { enabled = false },
      layout = { enabled = false },
      lazygit = {},
      notifier = {
        enabled = false,
        timeout = 3000, -- Keeping timeout in case you need it
      },
      notify = { enabled = false },
      picker = { enabled = false },
      profiler = { enabled = false },
      quickfile = { enabled = false },
      rename = { enabled = false },
      scope = { enabled = false },
      scratch = { enabled = false },
      scroll = { enabled = false },
      statuscolumn = { enabled = false },
      terminal = { enabled = false },
      toggle = { enabled = false },
      util = { enabled = false },
      win = { enabled = false },
      words = { enabled = false },
      zen = {
        toggles = {
          dim = true,
        },
        width = 120,
        height = 0,
        backdrop = { transparent = true, blend = 100 },
      },
    },
  },
}
