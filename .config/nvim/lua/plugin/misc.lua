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
  -- not working
  { 'glacambre/firenvim',
    lazy = true,
    event = "VeryLazy",
    build = ":call firenvim#install(0)"
  }
}
