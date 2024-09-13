local highlight_group = vim.api.nvim_create_augroup("YankHighlight", { clear = true })
vim.api.nvim_create_autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = "*",
})

local markdowngroup = vim.api.nvim_create_augroup("markdowngroup", { clear = true })

vim.api.nvim_create_autocmd("FileType", {
  group = markdowngroup,
  pattern = { "markdown" },
  callback = function()
    vim.opt_local.textwidth = 100
    vim.opt_local.wrap = true
    vim.opt_local.spell = true
    vim.opt_local.tabstop = 2
    vim.opt_local.softtabstop = 2
    vim.opt_local.shiftwidth = 2
    vim.opt_local.expandtab = true
    vim.opt.conceallevel = 2
  end,
})

