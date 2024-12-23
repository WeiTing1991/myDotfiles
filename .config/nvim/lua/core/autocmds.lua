vim.api.nvim_create_autocmd('TextYankPost', {
  desc = 'Highlight when yanking (copying) text',
  group = vim.api.nvim_create_augroup('kickstart-highlight-yank', { clear = true }),
  callback = function()
    vim.highlight.on_yank()
  end,
})

-- Markdwon
-- local markdowngroup = vim.api.nvim_create_augroup("markdowngroup", { clear = true })
--
-- vim.api.nvim_create_autocmd("FileType", {
--   group = markdowngroup,
--   pattern = { "markdown"},
--   callback = function()
--
--     -- vim.g.vim_markdown_frontmatter = 1
--     vim.opt_local.textwidth = 100
--     vim.opt_local.wrap = true
--     vim.opt_local.spell = true
--     vim.opt_local.conceallevel = 2
--     -- vim.opt_local.tabstop = 2
--     -- vim.opt_local.softtabstop = 2
--     -- vim.opt_local.shiftwidth = 2
--
--     -- vim.opt_local.number = false
--     -- vim.opt_local.relativenumber = false
--
--     -- vim.opt_local.expandtab = true
--     -- vim.g.markdown_fenced_languages = { "cpp", "python", "bash=sh", "json", "yaml", "vim", "lua"}
--     -- vim.keymap.set("n", "<leader>p", "<cmd>PasteImage<cr>", { desc = "Paste the image" })
--   end,
-- })
