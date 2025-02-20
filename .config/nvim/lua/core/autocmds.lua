local autocmd = vim.api.nvim_create_autocmd

vim.cmd([[
	filetype plugin indent on
]])

vim.api.nvim_create_autocmd("BufWritePre", {
  group = vim.api.nvim_create_augroup("wtNvimEditor", { clear = true }),
  pattern = { "*" }, -- Apply to all files
  callback = function()
    if vim.bo.filetype ~= "markdown" then
      vim.cmd([[ %s/\s\+$//e ]]) -- Trim trailing whitespace
    end
  end,
})

------------------------------------ highlight color ----------------------------------------:
vim.api.nvim_create_autocmd("TextYankPost", {
  desc = "Highlight when yanking (copying) text",
  group = vim.api.nvim_create_augroup("kickstart-highlight-yank", { clear = true }),
  callback = function()
    vim.highlight.on_yank()
  end,
})

-- some ui settings
vim.api.nvim_create_autocmd("BufWinEnter", {
  callback = function()
    vim.opt.guicursor = "n-v-c:block-Cursor,n-v-c-i:blinkon1,i:ver1000-Cursor,r-cr-o:hor100-cursor"

    -- optional
    -- vim.api.nvim_set_hl(0, "cursor", { background = "#eb6f92", foreground = "white"})

    -- winbar
    -- vim.api.nvim_set_hl(0, "WinBar", { fg = "#000000", bg = "#ffffff", bold = true })
    -- vim.api.nvim_set_hl(0, "WinBarNC", { fg = "#444444", bg = "#000000" }) -- Inactive window
  end,
})

-- TODO:
------------------------------------------- Autocmd for file type ------------------------------------

local set = vim.opt_local

--[[ Markdwon ]]
local mdgroup = vim.api.nvim_create_augroup("mdgroup", { clear = true })
vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
  group = mdgroup,
  pattern = "*.md",
  callback = function()
    set.shiftwidth = 2
    set.tabstop = 2
    set.softtabstop = 2
    set.textwidth = 150
    vim.opt.foldlevel = 99

    set.wrap = true
    set.spell = true
    set.number = false
    set.relativenumber = false
    -- set.conceallevel = 2
    vim.g.markdown_fenced_languages = { "cpp", "python", "bash=sh", "javascript", "json", "yaml", "vim", "lua" }
    -- vim.keymap.set("n", "<leader>p", "<cmd>PasteImage<cr>", { desc = "Paste the image" })
  end,
})

--[[ -- json ]]
local jsongroup = vim.api.nvim_create_augroup("jsongroup", { clear = true })
vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
  group = jsongroup,
  pattern = "*.json",
  callback = function()
    set.conceallevel = 0
    set.shiftwidth = 2
    set.tabstop = 2
    set.softtabstop = 2
    set.textwidth = 150

    vim.bo.filetype = "jsonc"
  end,
})

--[[ js ]]
local tsgroup = vim.api.nvim_create_augroup("tsgroup", { clear = true })
vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
  group = tsgroup,
  pattern = { "*.js", "*.ts", "*.tsx", "*.jsx" },
  callback = function()
    set.conceallevel = 0
    set.shiftwidth = 2
    set.tabstop = 2
    set.softtabstop = 2
    set.textwidth = 120
  end,
})

-- local cppgroup = vim.api.nvim_create_augroup("cppgroup", { clear = true })
-- vim.api.nvim_create_autocmd("FileType", {
--   group = cppgroup,
--   pattern = { "cpp", "c", "h" },
--   callback = function()
--     vim.opt_local.textwidth = 100
--     vim.opt_local.tabstop = 2
--     vim.opt_local.softtabstop = 2
--     vim.opt_local.shiftwidth = 2
--     vim.opt_local.expandtab = true
--
--     vim.keymap.set("n", "<leader>h", "<cmd>ClangdSwitchSourceHeader<cr>", { desc = "Switch between source and header" })
--   end,
-- })
--
