------------------------------------ highlight color ------------------------------------

local set = vim.opt_local

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
    -- Cusor LinNr color
    vim.api.nvim_set_hl(0, "NetrwDir", { ctermfg = "Blue", fg = "#698DDA" })

    -- vim.api.nvim_set_hl(0, "ShowBreak", { fg = "White", italic = true, underline = true })
    -- vim.api.nvim_set_hl(0, "LineNr", { fg = "#4b515d" })
    -- vim.api.nvim_set_hl(0, "CursorLinNr", { fg = "#000000" })

    -- optional
    vim.opt.guicursor = "n-v-c:block-Cursor,n-v-c-i:blinkon1,i:ver1000-Cursor,r-cr-o:hor100-cursor"
    -- vim.api.nvim_set_hl(0, "cursor", { background = "#eb6f92", foreground = "white"})

    --
    -- vim.o.winbar = "%#WinBarPath#%{expand('%:p:h:t')}/%#WinBarFile#%{expand('%:t')}"
    vim.o.winbar = "%f %m"

  end,
})

------------------------------------ Autocmd for specials file
-- TODO:

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

--[[ Markdwon ]]
local mdgroup = vim.api.nvim_create_augroup("mdgroup", { clear = true })
vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
  group = mdgroup,
  pattern = "*.md",
  callback = function()
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
  end,
})

-- vim.keymap.set("n", "<leader>p", "<cmd>PasteImage<cr>", { desc = "Paste the image" })
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
    set.textwidth = 180

    vim.bo.filetype = "jsonc"
  end,
})

