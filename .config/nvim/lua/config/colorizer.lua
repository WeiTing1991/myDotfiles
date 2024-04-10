require('colorizer').setup({
    user_default_options = {
        names = false, -- "Name" codes like Blue
        tailwind = true,
    },
})
-- keymapping
vim.keymap.set('n', '<leader>cz', vim.cmd.ColorizerToggle, { desc = 'toggle colorizer' })
