
vim.api.nvim_create_autocmd('TextYankPost', {
  desc = 'Highlight when yanking (copying) text',
  group = vim.api.nvim_create_augroup('kickstart-highlight-yank', { clear = true }),
  callback = function()
    vim.highlight.on_yank()
  end,
})

-- set the color
-- vim.api.nvim_create_autocmd("BufWinEnter", {
--   callback = function()
--     vim.api.nvim_set_hl(0, "LineNr", { fg = "#4b515d" })
--     -- vim.api.nvim_set_hl(0, "CursorLinNr", { fg = "#000000" })
--   end,
-- })
