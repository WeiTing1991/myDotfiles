return {
  {
    "tpope/vim-fugitive",
    lazy = true,
    event = { "BufReadPre", "BufNewFile" },
  },
  {
    "lewis6991/gitsigns.nvim",
    lazy = true,
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      require("gitsigns").setup({
        signs = {
          add = { text = "┃ " },
          change = { text = "┃ " },
          delete = { text = "_ " },
          topdelete = { text = "‾ " },
          changedelete = { text = "~ " },
          untracked = { text = "┆ " },
        },
        -- signs_staged = {
        --   add          = { text = '┃' },
        --   change       = { text = '┃' },
        --   delete       = { text = '_' },
        --   topdelete    = { text = '‾' },
        --   changedelete = { text = '~' },
        --   untracked    = { text = '┆' },
        -- },
        -- signs = {
        --   add = { text = "+" },
        --   change = { text = "+" },
        --   delete = { text = "_" },
        --   topdelete = { text = "‾" },
        --   changedelete = { text = "│" },
        --   untracked = { text = "┆" },
        -- },
        -- sings_staged = {
        --   add = { text = "+" },
        --   change = { text = "~" },
        --   delete = { text = "~" },
        --   topdelete = { text = "‾" },
        --   changedelete = { text = "│" },
        --   untracked = { text = "┆" },
        -- },
        signs_staged_enable = false,
        signcolumn = true, -- Toggle with `:Gitsigns toggle_signs`
        numhl = false, -- Toggle with `:Gitsigns toggle_numhl`
        linehl = false, -- Toggle with `:Gitsigns toggle_linehl`
        word_diff = false, -- Toggle with `:Gitsigns toggle_word_diff`
        watch_gitdir = {
          follow_files = true,
        },
        auto_attach = true,
        attach_to_untracked = true,
        current_line_blame = false,
        current_line_blame_opts = {
          virt_text = true,
          virt_text_pos = "eol", -- 'eol' | 'overlay' | 'right_align'
          delay = 1000,
          ignore_whitespace = false,
          virt_text_priority = 100,
          use_focus = true,
        },
        current_line_blame_formatter = "<author>, <author_time:%R> - <summary>",
        sign_priority = 100,
        update_debounce = 100,
        status_formatter = nil, -- Use default
        max_file_length = 40000, -- Disable if file is longer than this (in lines)
        preview_config = {
          style = "minimal",
          relative = "cursor",
          row = 0,
          col = 1,
        },
      })
    end,
  },
  {
    "kdheepak/lazygit.nvim",
    lazy = true,
    event = "VeryLazy",
    cmd = {
      "LazyGit",
      "LazyGitConfig",
      "LazyGitCurrentFile",
      "LazyGitFilter",
      "LazyGitFilterCurrentFile",
    },
  },
  {
    "sindrets/diffview.nvim",
    lazy = true,
    event = "VeryLazy",
    cmd = {
      "DiffviewOpen",
      "DiffviewClose",
      "DiffviewToggleFiles",
      "DiffviewFocusFiles",
      "DiffviewRefresh",
    },
    config = function()
      require("diffview").setup({})
    end,
  },
}
