require('mini.indentscope').setup({
    draw = {
        delay = 0,
        animation = function() return 0 end,
        priority = 2,
    },
    options = { border = 'top', try_as_border = true },
    symbol = '▏',
})

-- Disable for certain filetypes
vim.api.nvim_create_autocmd({ 'FileType' }, {
desc = 'Disable indentscope for certain filetypes',
callback = function()
  local ignore_filetypes = {
    'help',
    'lazy',
    'mason',
    'neo-tree',
    'NvimTree',
    'toggleterm',
    'Trouble',
  }
  if vim.tbl_contains(ignore_filetypes, vim.bo.filetype) then
    vim.b.miniindentscope_disable = true end
end,
})
