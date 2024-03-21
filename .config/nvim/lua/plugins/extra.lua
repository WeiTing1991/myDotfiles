return {
  -- makedown preview from browser
  {
    'iamcco/markdown-preview.nvim',
    cmd = { 'MarkdownPreviewToggle', 'MarkdownPreview', 'MarkdownPreviewStop' },
    event = 'VeryLazy',
    ft = { 'markdown' },
    build = function() vim.fn['mkdp#util#install']() end,
    config = function()
      vim.keymap.set('n', '<leader>mk', '<cmd>MarkdownPreviewToggle<cr>', { desc = 'markdown toggle' })
      vim.keymap.set('n', '<leader>mkp', '<cmd>MarkdownPreview<cr>', { desc = 'markdown preview' })
      vim.keymap.set('n', '<leader>mks', '<cmd>MarkdownPreviewStop<cr>', { desc = 'markdown stop' })
    end,
  },
  {
    'ThePrimeagen/harpoon',
    event = 'BufEnter',
    branch = 'harpoon2',
    dependencies = { 'nvim-lua/plenary.nvim' },
    config = function()
      local harpoon = require('harpoon')

      -- REQUIRED
      harpoon:setup()
      -- REQUIRED

      vim.keymap.set('n', '<leader>aa', function() harpoon:list():append() end, { desc = 'harpoon append' })
      vim.keymap.set(
        'n',
        '<leader>a',
        function() harpoon.ui:toggle_quick_menu(harpoon:list()) end,
        { desc = 'harpoon list' }
      )

      vim.keymap.set('n', '<leader>a1', function() harpoon:list():select(1) end)
      vim.keymap.set('n', '<leader>a1', function() harpoon:list():select(2) end)
      vim.keymap.set('n', '<leader>a2', function() harpoon:list():select(3) end)
      vim.keymap.set('n', '<leader>a3', function() harpoon:list():select(4) end)

      -- Toggle previous & next buffers stored within Harpoon list
      vim.keymap.set('n', '<C-S-p>', function() harpoon:list():prev() end, { desc = 'Go to pervious mark' })
      vim.keymap.set('n', '<C-S-n>', function() harpoon:list():next() end, { desc = 'Go to next mark' })
    end,
  },
  -- NOTE:
  -- REF https://github.com/nvim-neotest/neotest?tab=readme-ov-file

  {
    'folke/trouble.nvim',
    event = 'BufEnter',
    config = function()
      require('trouble').setup({
        icons = true,
      })
      vim.keymap.set({ 'n', 'v' }, '<leader>t', function() require('trouble').toggle() end, { desc = 'toggle trouble' })
      vim.keymap.set(
        'n',
        '<leader>tdw',
        function() require('trouble').toggle('workspace_diagnostics') end,
        { desc = 'workspace_diagnostics' }
      )
      vim.keymap.set(
        'n',
        '<leader>tdd',
        function() require('trouble').toggle('document_diagnostics') end,
        { desc = 'document_diagnostics' }
      )
      vim.keymap.set('n', '<leader>tq', function() require('trouble').toggle('quickfix') end, { desc = 'quickfix' })
      vim.keymap.set('n', '<leader>tl', function() require('trouble').toggle('loclist') end, { desc = 'loclist' })
      vim.keymap.set('n', 'tR', function() require('trouble').toggle('lsp_references') end, { desc = 'lsp_references' })
    end,
  },
}
