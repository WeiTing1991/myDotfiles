require('conform').setup({
  notify_on_error = false,
  format_on_save = {
    timeout_ms = 500,
    lsp_fallback = true,
  },
  formatters_by_ft = require('lsp.formatting'),
})
require('conform').formatters.clang_format = {
  command = 'clang-format',
  args = { '-assume-filename', vim.fn.expand('~/myDotfiles/.config/nvim/.clang-format') },
}
vim.keymap.set(
  'n',
  '<leader>m',
  function() require('conform').format({ async = true, lsp_fallback = true }) end,
  { desc = 'formattting' }
)
