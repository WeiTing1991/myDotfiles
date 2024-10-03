local highlight_group = vim.api.nvim_create_augroup("YankHighlight", { clear = true })
vim.api.nvim_create_autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = "*",
})

local markdowngroup = vim.api.nvim_create_augroup("markdowngroup", { clear = true })
local cppgroup = vim.api.nvim_create_augroup("cppgroup", { clear = true })

vim.api.nvim_create_autocmd("FileType", {
  group = markdowngroup,
  pattern = { "markdown" },
  callback = function()
    -- vim.g.markdown_recommended_style = 0
    vim.opt_local.textwidth = 80
    vim.opt_local.wrap = true
    vim.opt_local.spell = true
    vim.opt_local.tabstop = 2
    vim.opt_local.softtabstop = 2
    vim.opt_local.shiftwidth = 2
    vim.opt_local.expandtab = true
    vim.opt.conceallevel = 2
    vim.g.markdown_fenced_languages = { "cpp", "python", "bash=sh", "json", "yaml", "vim", "lua"}
    -- vim.keymap.set("n", "<leader>p", "<cmd>PasteImage<cr>", { desc = "Paste the image" })
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  group = cppgroup,
  pattern = { "cpp", "c", "h" },
  callback = function()
    vim.opt_local.textwidth = 100
    vim.opt_local.tabstop = 2
    vim.opt_local.softtabstop = 2
    vim.opt_local.shiftwidth = 2
    vim.opt_local.expandtab = true

    vim.keymap.set("n", "<leader>h", "<cmd>ClangdSwitchSourceHeader<cr>", { desc = "Switch between source and header" })
  end,
})
