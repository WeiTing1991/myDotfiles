local autocmd = vim.api.nvim_create_autocmd

autocmd("BufWritePre", {
  group = vim.api.nvim_create_augroup("wtc/trim_trailing_whitespace", { clear = true }),
  pattern = { "*" }, -- Apply to all files
  callback = function()
    if vim.bo.filetype ~= "markdown" then
      vim.cmd [[ %s/\s\+$//e ]] -- Trim trailing whitespace
    end
  end,
})

autocmd("TextYankPost", {
  desc = "Highlight when yanking (copying) text",
  group = vim.api.nvim_create_augroup("wtc/yank_highlight", { clear = true }),
  callback = function()
    vim.hl.on_yank { higroup = "YankHighlight", priority = 250 }
  end,
})

autocmd({ "bufenter", "bufwinenter" }, {
  group = vim.api.nvim_create_augroup("wtc/editor", { clear = true }),
  pattern = { "*" },
  callback = function()
    vim.cmd [[set formatoptions-=c formatoptions-=r formatoptions-=o]]
    --     vim.api.nvim_set_hl(0, "WinBar", { bg = "#303030" })
    --     vim.api.nvim_set_hl(0, "WinBarNC", { bg = "#1c1c1c" })
    --     vim.api.nvim_set_hl(0, "WinSeparator", { fg = "#ffffff" })
  end,
})

local filetype_exclude = { "help", "alpha", "dashboard", "nvim-tree", "Trouble", "lazy", "mason" }
autocmd("FileType", {
  group = vim.api.nvim_create_augroup("wt-local_detach_ufo", { clear = true }),
  pattern = filetype_exclude,
  callback = function()
    require("ufo").detach()
  end,
})

autocmd("FileType", {
  group = vim.api.nvim_create_augroup("wtc/close_with_q", { clear = true }),
  pattern = {
    "git",
    "diffview",
    "help",
    "man",
    "qf",
    "query",
    "scratch",
  },
  callback = function(args)
    vim.keymap.set("n", "q", "<cmd>quit<cr>", { buffer = args.buf })
  end,
})

local set = vim.opt_local

--[[ Markdwon ]]
local mdgroup = vim.api.nvim_create_augroup("mdgroup", { clear = true })
autocmd({ "BufRead", "BufNewFile" }, {
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
    set.conceallevel = 0
    -- vim.g.markdown_fenced_languages = { "cpp", "python", "bash=sh", "javascript", "json", "yaml", "vim", "lua" }
    -- vim.keymap.set("n", "<leader>p", "<cmd>PasteImage<cr>", { desc = "Paste the image" })
  end,
})

--[[ -- json ]]
local jsongroup = vim.api.nvim_create_augroup("jsongroup", { clear = true })
autocmd({ "BufRead", "BufNewFile" }, {
  group = jsongroup,
  pattern = "*.json",
  callback = function()
    set.conceallevel = 0
    set.shiftwidth = 2
    set.tabstop = 2
    set.softtabstop = 2
    set.textwidth = 150
  end,
})

--[[ ts/js ]]
local tsgroup = vim.api.nvim_create_augroup("tsgroup", { clear = true })
autocmd({ "BufRead", "BufNewFile" }, {
  group = tsgroup,
  pattern = { "*.js", "*.ts", "*.tsx", "*.jsx" },
  callback = function()
    set.conceallevel = 0
    set.shiftwidth = 2
    set.tabstop = 2
    set.softtabstop = 2
    set.textwidth = 120
    -- vim.keymap.set("n", "<S-l>ta", ":TSToolsAddMissingImports<cr>", { desc = "add missing import" })
    -- vim.keymap.set("n", "<S-l>tr", ":TSToolRemoveUnusedImports<cr>", { desc = "remove unused import" })
  end,
})

-- local cppgroup = vim.api.nvim_create_augroup("cppgroup", { clear = true })
-- autocmd("FileType", {
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
