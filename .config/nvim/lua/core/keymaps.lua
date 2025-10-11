
-- Remap for dealing with word wrap and adding jumps to the jumplist.
vim.keymap.set("n", "j", [[(v:count > 0 ? 'm`' . v:count : 'g') . 'j']], { expr = true })
vim.keymap.set("n", "k", [[(v:count > 1 ? 'm`' . v:count : 'g') . 'k']], { expr = true })

-- Disable some default keys
vim.keymap.set({ "n", "v" }, "<Space>", "<Nop>", { silent = true })
vim.keymap.set({ "n", "v" }, "<F1>", "<Nop>", { silent = true })
vim.keymap.set({ "n", "v" }, "<C-z>", "<Nop>", { silent = true })
vim.keymap.set({ "n", "v" }, "<C-m>", "<C-m>", { silent = true })

vim.keymap.set("n", "<C-q>", function() vim.cmd(":bw!") end, { desc = "Close current buffer and window" })

-- Clear search highlights
vim.keymap.set("n", "<Esc>", ":noh<CR>", { desc = "Clear highlights" })
vim.keymap.set("n", "<C-c>", ":noh<CR>", { desc = "Clear highlights" })

-- switch between buffer
vim.keymap.set("n", "<C-I>", "<C-I>", { noremap = true })
vim.keymap.set("n", "<C-O>", "<C-O>", { noremap = true })
vim.keymap.set("n", "<C-i>", ":bp<cr>", { desc = "Prevous buffer" })
vim.keymap.set("n", "<C-o>", ":bn<cr>", { desc = "Next buffer" })
-- fallback
vim.keymap.set("n", "<leader>o", ":bnext<cr>", { desc = "Next buffer" })
vim.keymap.set("n", "<leader>i", ":bprevious<cr>", { desc = "Prevous buffer" })

-- vim.keymap.set("n", "<C-M>", "<C-M>", { noremap = true })
-- vim.keymap.set("n", "<Tab>", "za", { noremap = true })


-- Comment
vim.api.nvim_set_keymap("n", "-", "gcc", { desc = "comment" })
vim.api.nvim_set_keymap("v", "-", "gc", { desc = "comment" })
vim.api.nvim_set_keymap("n", "_", "gbc", { desc = "comment blockwise" })
vim.api.nvim_set_keymap("v", "_", "gb", { desc = "comment blockwise" })

-- Leave insert mode
vim.keymap.set("i", "<C-c>", "<Esc>")
vim.keymap.set("i", "<Esc>", "<C-c>")
vim.keymap.set("i", "jk", "<Esc>")

-- Move the highlighted line down/up
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv", { desc = "Move selection down" })
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv", { desc = "Move selection up" })

-- Join current line with the line below
vim.keymap.set("n", "J", "mzJ`z", { desc = "Join lines" })

-- Keep cursor centered when scrolling
vim.keymap.set("n", "<C-d>", "<C-d>zz", { desc = "Scroll half page down" })
vim.keymap.set("n", "<C-u>", "<C-u>zz", { desc = "Scroll half page up" })
vim.keymap.set("n", "G", "Gzz", { desc = "Scroll to bottom" })

-- Keep cursor centered when jumping with 'n' and 'N'
vim.keymap.set("n", "n", "nzzzv", { desc = "Jump to next match" })
vim.keymap.set("n", "N", "Nzzzv", { desc = "Jump to previous match" })

-- Normal mode - works fine
vim.keymap.set("n", "<C-h>", "<C-w>h", { desc = "Move to left buffer" })
vim.keymap.set("n", "<C-l>", "<C-w>l", { desc = "Move to right buffer" })
vim.keymap.set("n", "<C-k>", "<C-w>k", { desc = "Move to up buffer" })
vim.keymap.set("n", "<C-j>", "<C-w>j", { desc = "Move to down buffer" })

-- Terminal mode - needs escape first
vim.keymap.set("t", "<C-h>", "<C-\\><C-n><C-w>h", { desc = "Move to left buffer" })
vim.keymap.set("t", "<C-l>", "<C-\\><C-n><C-w>l", { desc = "Move to right buffer" })
vim.keymap.set("t", "<C-k>", "<C-\\><C-n><C-w>k", { desc = "Move to up buffer" })
vim.keymap.set("t", "<C-j>", "<C-\\><C-n><C-w>j", { desc = "Move to down buffer" })
vim.keymap.set({ "n", "t" }, "<C-S-Left>", "<C-w>>", { desc = "resize +2" })
vim.keymap.set({ "n", "t" }, "<C-S-Right>", "<C-w><", { desc = "resize -2" })
vim.keymap.set({ "n", "t" }, "<C-S-Up>", "<C-w>+", { desc = "vertical resize -2" })
vim.keymap.set({ "n", "t" }, "<C-S-Down>", "<C-w>-", { desc = "vertical resize -2" })

vim.api.nvim_set_keymap("i", "<C-BS>", "<C-W>", { noremap = true })

-- tabs
-- vim.keymap.set("n", "<C-S-i>", ":tabnext<CR>", { noremap = true, silent = true, desc = "Next tab" })
-- vim.keymap.set("n", "<C-S-o>", ":tabprevious<CR>", { noremap = true, silent = true, desc = "Previous tab" })

-- windows
-- vim.keymap.set("n", "<C-'>", function()
--   require("custom_plugins.toggle_maximize_window").toggle_maximize_window()
-- end, { desc = "Toggle maximize buffer" })
--
-- vim.keymap.set({ "n", "v" }, "<C-'>", function()
--   require("core.toogle_max").toggle_maximize_window()
-- end, { desc = "Toggle maximize buffer" })

vim.keymap.set("n", "<C-w>'", "<C-w>v", { desc = "split vertically" })
vim.keymap.set("n", "<C-w>5", "<C-w>s", { desc = "split horizontally" })

-- indenting
vim.keymap.set("v", "<", "<gv")
vim.keymap.set("v", ">", ">gv")

-- Replace the word throughout the file
vim.keymap.set(
  { "n", "v" },
  "<leader>s",
  [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]],
  { desc = "Replace word throughout file" }
)

-- Change root to current file dir
vim.keymap.set("n", "<leader>cd", ":cd %:p:h<cr>", { desc = "cd current file dir" })

-- Toggle spelling
function ToggleSpellCheck()
  if vim.o.spell then
    vim.o.spell = false
    print("Spelling check OFF.")
  else
    vim.o.spell = true
    print("Spelling check ON.")
  end
end
vim.keymap.set("n", "<leader>tp", ToggleSpellCheck, { desc = "Spell check" })

-- Optional
vim.keymap.set("x", "p", [["_dP]], { desc = "Replace selection with system clipboard content" })
-- vim.keymap.set("n", "<leader><F2>", ":InspectTree<CR>", { desc = "Inspect Tree" })
