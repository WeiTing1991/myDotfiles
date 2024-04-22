-- [[ Highlight on yank ]]
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup("YankHighlight", { clear = true })
vim.api.nvim_create_autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = "*",
})

local weitinggroup = vim.api.nvim_create_augroup("weiting", { clear = true })
vim.api.nvim_create_autocmd({ "BufWritePre" }, {
  group = weitinggroup,
  pattern = "*",
  command = [[%s/\s\+$//e]],
})

local markdowngroup = vim.api.nvim_create_augroup("markdowngroup", { clear = true })
-- spell check
vim.api.nvim_create_autocmd({ "FileType" }, {
  group = markdowngroup,
  pattern = "markdown",
  command = "setlocal spell",
})

vim.api.nvim_create_autocmd({ "FileType" }, {
  group = markdowngroup,
  pattern = "markdown",
  command = "set linebreak",
})

-- vim.api.nvim_create_autocmd({ "BufWritePre" }, {
--   group = markdowngroup,
--   pattern = "*.md",
--   callback = function()
--     vim.opt.wrap = true
--   end,
-- })

vim.api.nvim_create_autocmd({ "FileType" }, {
  group = markdowngroup,
  pattern = "markdown",
  command = "setlocal complete+=kspell",
})

vim.api.nvim_create_autocmd({ "FileType" }, {
  group = markdowngroup,
  pattern = "markdown",
  callback = function()
    vim.diagnostic.config { virtual_text = false }
  end,
})

vim.api.nvim_create_autocmd({ "FileType" }, {
  group = markdowngroup,
  pattern = "markdown",
  command = "highlight SpellBad cterm=underline ctermbg=red",
})

-- higjlight the templ
vim.api.nvim_create_autocmd({ "FileType" }, {
  pattern = "templ",
  callback = function()
    vim.cmd "TSBufEnable highlight"
  end,
})
