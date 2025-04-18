local autocmd = vim.api.nvim_create_autocmd

vim.cmd [[
	filetype plugin indent on
]]

vim.api.nvim_create_autocmd("BufWritePre", {
  group = vim.api.nvim_create_augroup("wtNvimEditor", { clear = true }),
  pattern = { "*" }, -- Apply to all files
  callback = function()
    if vim.bo.filetype ~= "markdown" then
      vim.cmd [[ %s/\s\+$//e ]] -- Trim trailing whitespace
    end
  end,
})

------ highlight color -------
vim.api.nvim_create_autocmd("TextYankPost", {
  desc = "Highlight when yanking (copying) text",
  group = vim.api.nvim_create_augroup("kickstart-highlight-yank", { clear = true }),
  callback = function()
    vim.highlight.on_yank()
  end,
})
