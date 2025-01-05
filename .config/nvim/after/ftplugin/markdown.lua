--[[ Markdwon ]]

local set = vim.opt_local

set.shiftwidth = 2
set.tabstop = 2
set.softtabstop = 2
set.textwidth = 200

set.wrap = true
set.spell = true
set.number = false
set.relativenumber = false
-- set.conceallevel = 2


vim.g.markdown_fenced_languages = { "cpp", "python", "bash=sh", "json", "yaml", "vim", "lua"}

-- vim.keymap.set("n", "<leader>p", "<cmd>PasteImage<cr>", { desc = "Paste the image" })


