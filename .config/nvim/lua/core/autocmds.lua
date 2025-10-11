local autocmd = vim.api.nvim_create_autocmd
local set = vim.opt_local

autocmd("BufWritePre", {
  group = vim.api.nvim_create_augroup("wtc/trim_trailing_whitespace", { clear = true }),
  pattern = { "*" }, -- Apply to all files
  callback = function()
    if vim.bo.filetype ~= "markdown" then
      vim.cmd([[ %s/\s\+$//e ]]) -- Trim trailing whitespace
    end
  end,
})

autocmd("TextYankPost", {
  desc = "Highlight when yanking (copying) text",
  group = vim.api.nvim_create_augroup("wtc/yank_highlight", { clear = true }),
  callback = function()
    vim.hl.on_yank()
  end,
})

vim.api.nvim_create_autocmd({ "WinEnter", "BufEnter" }, {
  callback = function()
    vim.wo.winhl = "WinSeparator:WinSeparatorFocused"
  end,
})

vim.api.nvim_create_autocmd({ "WinLeave", "BufLeave" }, {
  callback = function()
    vim.wo.winhl = "WinSeparator:WinSeparator"
  end,
})

--[[ python ]]
local pygroup = vim.api.nvim_create_augroup("pygroup", { clear = true })
autocmd({ "BufRead", "BufNewFile" }, {
  group = pygroup,
  pattern = "*.py",
  callback = function()
    set.conceallevel = 0
    set.shiftwidth = 4
    set.tabstop = 4
    set.softtabstop = 4
  end,
})
