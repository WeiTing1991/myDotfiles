require('trouble').setup({
    icons = true,
})
vim.keymap.set({ 'n', 'v' }, '<leader>t',
    function()
        require('trouble').toggle()
    end,
    { desc = 'toggle trouble' }
)
vim.keymap.set('n', '<leader>tl',
    function()
        require('trouble').toggle('loclist')
    end,
    { desc = 'loclist' }
)
vim.keymap.set('n', '<leader>tq',
    function()
        require('trouble').toggle('quickfix')
    end,
    { desc = 'quickfix' }
)


vim.keymap.set('n', '<leader>tdw',
    function()
        require('trouble').toggle('workspace_diagnostics')
    end,
    { desc = 'workspace_diagnostics' }
)
vim.keymap.set('n', '<leader>tdd',
    function()
        require('trouble').toggle('document_diagnostics')
    end,
    { desc = 'document_diagnostics' }
)
vim.keymap.set('n', 'tR',
    function()
        require('trouble').toggle('lsp_references')
    end,
    { desc = 'lsp_references' }
)
