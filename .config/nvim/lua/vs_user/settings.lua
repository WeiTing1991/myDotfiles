vim.g.mapleader = " "
vim.g.maplocalleader = ","

vim.keymap.set("v", "<", "<gv")
vim.keymap.set("v", ">", ">gv")
-- vim.keymap.set("n", "n", "nzzzv", { desc = "Jump to next match" })
-- vim.keymap.set("n", "N", "Nzzzv", { desc = "Jump to previous match" })
vim.keymap.set("n", "J", "mzJ`z", { desc = "Join lines" })

vim.keymap.set("n", "<Esc>", ":noh<CR>", { desc = "Clear highlights" })
vim.keymap.set("n", "<C-c>", ":noh<CR>", { desc = "Clear highlights" })

-- Scroll
vim.keymap.set("n", "<C-d>", "<C-d>zz", { desc = "Scroll half page down" })
vim.keymap.set("n", "<C-u>", "<C-u>zz", { desc = "Scroll half page up" })
vim.keymap.set("n", "G", "Gzz", { desc = "Scroll to bottom" })

-- Leave insert mode
vim.keymap.set("i", "<C-c>", "<Esc>")
vim.keymap.set("i", "<Esc>", "<C-c>")

-- Options
local options ={
  clipboard = "unnamedplus",
  incsearch = false,
  ignorecase = true,
  smartcase = true,
  hlsearch = true,
  backup = false,
  swapfile = false,
  -- scrolloff = 50,
  sidescrolloff = 0,
}

for k, v in pairs(options) do
  vim.opt[k] = v
end

-- globals
local globals = {
  prev_buffer = nil,
  next_buffer = nil,
  -- disable auto format
  autoformat = false,
}

for k, v in pairs(globals) do
  vim.g[k] = v
end

--[[
    surr*ound_words             ysiw)           (surround_words)
    *make strings               ys$"            "make strings"
    [delete ar*ound me!]        ds]             delete around me!
    remove <b>HTML t*ags</b>    dst             remove HTML tags
    'change quot*es'            cs'"            "change quotes"
    <b>or tag* types</b>        csth1<CR>       <h1>or tag types</h1>
    delete(functi*on calls)     dsf             function calls ]]

require "vs_user.keybinding"
require "vs_user.lazy"

local autocmd = vim.api.nvim_create_autocmd
local set = vim.opt_local

autocmd("TextYankPost", {
  desc = "Highlight when yanking (copying) text",
  group = vim.api.nvim_create_augroup("wtc/yank_highlight", { clear = true }),
  callback = function()
    vim.hl.on_yank()
  end,
})
