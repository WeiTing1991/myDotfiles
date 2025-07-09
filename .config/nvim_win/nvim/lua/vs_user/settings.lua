vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- basic keybinding
-- Is Working?
-- Remap for dealing with word wrap and adding jumps to the jumplist.
-- vim.keymap.set('n', 'j', [[(v:count > 0 ? 'm`' . v:count : 'g') . 'j']], { expr = true })
-- vim.keymap.set('n', 'k', [[(v:count > 1 ? 'm`' . v:count : 'g') . 'k']], { expr = true })

vim.keymap.set("v", "<", "<gv")
vim.keymap.set("v", ">", ">gv")
vim.keymap.set("n", "n", "nzzzv", { desc = "Jump to next match" })
vim.keymap.set("n", "N", "Nzzzv", { desc = "Jump to previous match" })
vim.keymap.set("n", "J", "mzJ`z", { desc = "Join lines" })


vim.keymap.set("n", "<Esc>", ":noh<CR>", { desc = "Clear highlights" })
vim.keymap.set("n", "<C-c>", ":noh<CR>", { desc = "Clear highlights" })

-- comment(use vscode default)
-- vim.api.nvim_set_keymap("n", "-", "gcc", { desc = "comment" })
-- vim.api.nvim_set_keymap("v", "-", "gc", { desc = "comment" })
-- vim.api.nvim_set_keymap("n", "_", "gbc", { desc = "comment blockwise" })
-- vim.api.nvim_set_keymap("v", "_", "gb", { desc = "comment blockwise" })

-- -- Move the highlighted line down
-- vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv", { desc = "Move selection down" })
-- -- Move the highlighted line up
-- vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv", { desc = "Move selection up" })

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
  scrolloff = 50,
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

-- require "vs_user.lazy"
require "vs_user.keybinding"
