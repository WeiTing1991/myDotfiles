-- root_dir = vim.fn.stdpath "data" .. "/sessions/", -- Root dir where sessions will be stored

require("auto-session").setup {
  suppressed_dirs = { '~/', '~/Project', '~/Downloads', '/' },
  auto_restore = false,

}

vim.keymap.set("n", '<leader>w', '<cmd>SessionRestore<CR>', {desc = 'Session search'} )
vim.keymap.set("n", '<leader>wr', '<cmd>SessionSearch<CR>', {desc = 'Session search'} )
vim.keymap.set("n", '<leader>s', '<cmd>SessionSave<CR>', {desc = 'Save session'} )
vim.keymap.set("n", '<leader>wd', '<cmd>SessioDelete<CR>', {desc = 'Save session'} )
