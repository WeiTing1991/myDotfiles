local utils = require("core.utils")
local map = vim.keymap.set

-- Remap for dealing with word wrap and adding jumps to the jumplist.
map("n", "j", [[(v:count > 0 ? 'm`' . v:count : 'g') . 'j']], { expr = true })
map("n", "k", [[(v:count > 1 ? 'm`' . v:count : 'g') . 'k']], { expr = true })

-- Disable some default keys
map({ "n", "v" }, "<Space>", "<Nop>", { silent = true })
map({ "n", "v" }, "<F1>", "<Nop>", { silent = true })
map({ "n", "v" }, "<C-z>", "<Nop>", { silent = true })
map({ "n", "v" }, "<C-m>", "<C-m>", { silent = true })
map({ "n", "v" }, "<Tab>", "<Tab>", { silent = true })

-- Close buffer
map("n", "<C-q>", "<cmd>bw!<cr>", { desc = "Close current buffer and window" })

-- Clear search highlights
map("n", "<Esc>", "<cmd>noh<cr>", { desc = "Clear highlights" })
map("n", "<C-c>", "<cmd>noh<cr>", { desc = "Clear highlights" })

-- Buffer navigation
-- map("n", "]b", "<cmd>bnext<cr>", { desc = "Next buffer" })
-- map("n", "[b", "<cmd>bprevious<cr>", { desc = "Previous buffer" })
map("n", "<leader>o", "<cmd>bnext<cr>", { desc = "Next buffer" })
map("n", "<leader>i", "<cmd>bprevious<cr>", { desc = "Previous buffer" })

-- Preserve jumplist
map("n", "<C-I>", "<C-I>", { noremap = true })
map("n", "<C-O>", "<C-O>", { noremap = true })
-- map("n", "<C-/>", "<C-/>", { noremap = true })

-- Comment (relies on Comment.nvim gc/gb mappings)
vim.api.nvim_set_keymap("n", "<C-/>", "gcc", { desc = "comment" })
vim.api.nvim_set_keymap("n", "-", "gcc", { desc = "comment" })
vim.api.nvim_set_keymap("v", "-", "gc", { desc = "comment" })

vim.api.nvim_set_keymap("n", "<C-S-/>", "gcc", { desc = "comment" })
vim.api.nvim_set_keymap("n", "_", "gbc", { desc = "comment blockwise" })
vim.api.nvim_set_keymap("v", "_", "gb", { desc = "comment blockwise" })

-- Leave insert mode
map("i", "<C-c>", "<Esc>")
map("i", "<Esc>", "<C-c>")
map("i", "jk", "<Esc>")

-- Move selection up/down in visual mode
map("v", "J", ":m '>+1<CR>gv=gv", { desc = "Move selection down" })
map("v", "K", ":m '<-2<CR>gv=gv", { desc = "Move selection up" })

-- Join current line with the line below
map("n", "J", "mzJ`z", { desc = "Join lines" })

-- Keep cursor centered when scrolling
map("n", "<C-d>", "<C-d>zz", { desc = "Scroll half page down" })
map("n", "<C-u>", "<C-u>zz", { desc = "Scroll half page up" })
map("n", "G", "Gzz", { desc = "Scroll to bottom" })

-- Keep cursor centered when jumping with 'n' and 'N'
map("n", "n", "nzzzv", { desc = "Jump to next match" })
map("n", "N", "Nzzzv", { desc = "Jump to previous match" })

-- Window navigation
map("n", "<C-h>", "<C-w>h", { desc = "Move to left buffer" })
map("n", "<C-l>", "<C-w>l", { desc = "Move to right buffer" })
map("n", "<C-k>", "<C-w>k", { desc = "Move to up buffer" })
map("n", "<C-j>", "<C-w>j", { desc = "Move to down buffer" })

-- Terminal mode
map("t", "<C-h>", "<C-\\><C-n><C-w>h", { desc = "Move to left buffer" })
map("t", "<C-l>", "<C-\\><C-n><C-w>l", { desc = "Move to right buffer" })
map("t", "<C-k>", "<C-\\><C-n><C-w>k", { desc = "Move to up buffer" })
map("t", "<C-j>", "<C-\\><C-n><C-w>j", { desc = "Move to down buffer" })

-- Window resize
map({ "n", "t" }, "<C-S-Left>", "<C-w>>", { desc = "resize +2" })
map({ "n", "t" }, "<C-S-Right>", "<C-w><", { desc = "resize -2" })
map({ "n", "t" }, "<C-S-Up>", "<C-w>+", { desc = "vertical resize +2" })
map({ "n", "t" }, "<C-S-Down>", "<C-w>-", { desc = "vertical resize -2" })

-- Backspace in insert
map("i", "<C-BS>", "<C-W>", { noremap = true })

-- Tabs
map("n", "<leader>tk", "<cmd>tabnext<cr>", { noremap = true, silent = true, desc = "Next tab" })
map("n", "<leader>tn", "<cmd>tabnew<cr>", { noremap = true, silent = true, desc = "New tab" })
map("n", "<leader>tq", "<cmd>tabclose<cr>", { noremap = true, silent = true, desc = "Close tab" })
map("n", "<leader>tj", "<cmd>tabprevious<cr>", { noremap = true, silent = true, desc = "Previous tab" })

-- Windows
map({ "n", "t" }, "<leader>'", function()
  utils.toggle_maximize_window()
end, { desc = "Toggle maximize buffer" })

map("n", "<C-w>'", "<C-w>v", { desc = "split vertically" })
map("n", "<C-w>5", "<C-w>s", { desc = "split horizontally" })

-- Indenting
map("v", "<", "<gv")
map("v", ">", ">gv")

-- Replace the word throughout the file
map(
  { "n", "v" },
  "<leader>S",
  [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gcI<Left><Left><Left>]],
  { desc = "Simple search and replace" }
)

-- Replace in quickfix
map("n", "<leader>s", function()
  local search = vim.fn.input("Search: ")
  if search == "" then
    return
  end
  local replace = vim.fn.input("Replace: ")
  vim.cmd("cdo s/" .. vim.fn.escape(search, "/\\") .. "/" .. vim.fn.escape(replace, "/\\") .. "/gc | update")
end, { desc = "Search and replace" })

-- Toggle spelling (fastspell)
map("n", "<leader>tp", function()
  vim.g.spell_enabled = not vim.g.spell_enabled
  if vim.g.spell_enabled then
    local first_line = vim.fn.line("w0") - 1
    local last_line = vim.fn.line("w$")
    local ok, fastspell = pcall(require, "fastspell")
    if ok then
      fastspell.sendSpellCheckRequest(first_line, last_line)
    end
    vim.notify("Spell check enabled", vim.log.levels.INFO)
  else
    local ok, fastspell = pcall(require, "fastspell")
    if ok then
      fastspell.sendSpellCheckRequest(0, 0)
    end
    vim.notify("Spell check disabled", vim.log.levels.INFO)
  end
end, { desc = "Spell check" })
