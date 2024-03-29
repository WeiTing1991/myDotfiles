return {
  {
    'tpope/vim-fugitive',
    event = 'BufEnter',
    config = function()
      vim.keymap.set('n', '<leader>gs', vim.cmd.Git, { desc = 'Git status' })
      vim.keymap.set('n', 'gu', '<cmd>diffget //2<cr>', { desc = 'show different' })
      vim.keymap.set('n', 'gh', '<cmd>diffget //3<cr>', { desc = 'show different' })

      -- local ThePrimeagen_Fugitive = vim.api.nvim_create_augroup("ThePrimeagen_Fugitive", {})
      -- local autocmd = vim.api.nvim_create_autocmd
      -- autocmd("BufWinEnter", {
      --     group = ThePrimeagen_Fugitive,
      --     pattern = "*",
      --     callback = function()
      --         if vim.bo.ft ~= "fugitive" then
      --             return
      --         end
      --
      --         local bufnr = vim.api.nvim_get_current_buf()
      --         local opts = {buffer = bufnr, remap = false}
      --         vim.keymap.set("n", "<leader>p", function()
      --             vim.cmd.Git('push')
      --         end, opts)
      --
      --         -- rebase always
      --         vim.keymap.set("n", "<leader>P", function()
      --             vim.cmd.Git({'pull',  '--rebase'})
      --         end, opts)
      --
      --
      --         vim.keymap.set("n", "<leader>t", ":Git push -u origin ", opts);
      --     end,
      -- })
      --
      --
    end,
  },
  {
    'lewis6991/gitsigns.nvim',
    event = 'BufEnter',
    config = function()
      require('gitsigns').setup({
        signs = {
          add = { text = '│' },
          change = { text = '│' },
          delete = { text = '_' },
          topdelete = { text = '‾' },
          changedelete = { text = '~' },
          untracked = { text = '┆' },
        },
        signcolumn = true, -- Toggle with `:Gitsigns toggle_signs`
        numhl = false, -- Toggle with `:Gitsigns toggle_numhl`
        linehl = false, -- Toggle with `:Gitsigns toggle_linehl`
        word_diff = false, -- Toggle with `:Gitsigns toggle_word_diff`
        watch_gitdir = {
          follow_files = true,
        },
        auto_attach = true,
        attach_to_untracked = false,
        current_line_blame = false, -- Toggle with `:Gitsigns toggle_current_line_blame`
        current_line_blame_opts = {
          virt_text = true,
          virt_text_pos = 'eol', -- 'eol' | 'overlay' | 'right_align'
          delay = 1000,
          ignore_whitespace = false,
          virt_text_priority = 100,
        },
        current_line_blame_formatter = '<author>, <author_time:%Y-%m-%d> - <summary>',
        sign_priority = 6,
        update_debounce = 100,
        status_formatter = nil, -- Use default
        max_file_length = 40000, -- Disable if file is longer than this (in lines)
        preview_config = {
          -- Options passed to nvim_open_win
          border = 'single',
          style = 'minimal',
          relative = 'cursor',
          row = 0,
          col = 1,
        },
        yadm = {
          enable = false,
        },
      })
      -- NOTE: https://github.com/lewis6991/gitsigns.nvim
      --
      -- keymaps
      vim.keymap.set('n', '<leader>gh', ':Gitsign preview_hunk<CR>', { desc = 'Preview hunk' })
      vim.keymap.set('n', '<leader>gt', ':Gitsign toggle_current_line_blame<CR>', { desc = 'Preview hunk' })
    end,
  },
}
