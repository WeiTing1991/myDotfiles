vim.api.nvim_set_hl(0, 'MiniIndentscopeSymbol', { fg = "#e5e9f0", default = true, })
vim.api.nvim_set_hl(0, 'MiniIndentscopeSymbolOff', { default = true, link = 'MiniIndentscopeSymbol' })

require('mini.indentscope').setup({
  draw = {
    delay = 0,
    animation = function() return 0 end,
    priority = 2,
  },

  symbol = '▏',
  options = { border = 'top', try_as_border = true },
})

-- Disable for certain filetypes
vim.api.nvim_create_autocmd("FileType", {
  pattern = {
    "lspinfo",
    "checkhealth",
    "help",
    "lazy",
    "mason",
    "telescope",
    "markdown",
    "nvimtree",
    "oil",
    "fzf",
    -- "toggleterm",
  },
  callback = function()
    vim.b.miniindentscope_disable = true
  end,
})
