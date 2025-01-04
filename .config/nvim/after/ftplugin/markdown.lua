-- local markdowngroup = vim.api.nvim_create_augroup("markdowngroup", { clear = true })
-- vim.api.nvim_create_autocmd("FileType", {
--   group = markdowngroup,
--   pattern = "markdown",
--   callback = function()
--     -- vim.g.vim_markdown_frontmatter = 1
--     -- vim.opt_local.expandtab = true
--     -- vim.g.markdown_fenced_languages = { "cpp", "python", "bash=sh", "json", "yaml", "vim", "lua"}
--     -- vim.keymap.set("n", "<leader>p", "<cmd>PasteImage<cr>", { desc = "Paste the image" })
--   end,
-- })


local set = vim.opt_local

set.shiftwidth = 2
set.tabstop = 2
set.softtabstop = 2
set.textwidth = 100

set.wrap = true
set.spell = true
set.number = false
set.relativenumber = false
-- set.conceallevel = 2


