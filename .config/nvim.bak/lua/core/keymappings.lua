----------------------------------- default keymap -------------------------------------
local map = vim.keymap.set

-- disable space key
vim.keymap.set({ "n", "v" }, "<Space>", "<Nop>", { silent = true })
vim.keymap.set({ "n", "v" }, "<C-z>", "<Nop>", { silent = true })
vim.keymap.set({ "n", "v" }, "<C-i>", "<Nop>", { silent = true })
vim.keymap.set({ "n", "v" }, "<C-o>", "<Nop>", { silent = true })
vim.keymap.set({ "n", "v" }, "<F1>", "<Nop>", { silent = true })

-- toggle spelling
function ToggleSpellCheck()
  if vim.o.spell then
    vim.o.spell = false
    print "Spelling check OFF."
  else
    vim.o.spell = true
    print "Spelling check ON."
  end
end

vim.keymap.set("n", "<leader>tp", ToggleSpellCheck, { desc = "Spell check" })

-- fold
vim.keymap.set("n", "<Tab>", function()
  -- Get the current line number
  local line = vim.fn.line "."
  -- Get the fold level of the current line
  local foldlevel = vim.fn.foldlevel(line)
  if foldlevel == 0 then
    vim.notify("No fold found", vim.log.levels.INFO)
  else
    vim.cmd "normal! za"
  end
end, { desc = "Toggle fold" })

-- buffer
vim.keymap.set("n", "<leader>o", ":bnext<cr>", { desc = "Next buffer" })
vim.keymap.set("n", "<leader>i", ":bprevious<cr>", { desc = "Prevous buffer" })

vim.keymap.set("n", "<C-o>", ":bn<cr>", { desc = "Next buffer" })
vim.keymap.set("n", "<C-i>", ":bp<cr>", { desc = "Prevous buffer" })

vim.keymap.set("n", "<leader>q", function()
  vim.cmd ":bw"
end, { desc = "Close current buffer and window" })

-- clear search highlights
vim.keymap.set("n", "<Esc>", ":nohl<CR>", { desc = "Clear highlights" })

-- Move the highlighted line down
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv", { desc = "Move selection down" })
-- Move the highlighted line up
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

-- move and split between neovim buffers
vim.keymap.set({ "n", "t" }, "<C-h>", "<C-w>h", { desc = "move to left buffer" })
vim.keymap.set({ "n", "t" }, "<C-l>", "<C-w>l", { desc = "move to right buffer" })
vim.keymap.set({ "n", "t" }, "<C-k>", "<C-w>k", { desc = "move to up buffer" })
vim.keymap.set({ "n", "t" }, "<C-j>", "<C-w>j", { desc = "move to down buffer" })

-- windows
vim.keymap.set("n", "<C-'>", function()
  require("custom_plugins.toggle_maximize_window").toggle_maximize_window()
end, { desc = "Toggle maximize buffer" })

vim.keymap.set("n", "<C-w>5", "<C-w>v", { desc = "split vertically" })
vim.keymap.set("n", "<C-w>'", "<C-w>s", { desc = "split horizontally" })

-- comment
vim.api.nvim_set_keymap("n", "-", "gcc", { desc = "comment" })
vim.api.nvim_set_keymap("v", "-", "gc", { desc = "comment" })

vim.api.nvim_set_keymap("n", "_", "gbc", { desc = "comment blockwise" })
vim.api.nvim_set_keymap("v", "_", "gb", { desc = "comment blockwise" })

vim.keymap.set("i", "<C-c>", "<Esc>")
vim.keymap.set("i", "<Esc>", "<C-c>")
vim.keymap.set("i", "jk", "<Esc>")

-- indenting
vim.keymap.set("v", "<", "<gv")
vim.keymap.set("v", ">", ">gv")

-- Replace the word throughout the file
vim.keymap.set(
  "n",
  "<leader>s",
  [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]],
  { desc = "Replace word throughout file" }
)
-- inoremap ( ()<Left>
-- inoremap { {}<Left>
-- inoremap [ []<Left>
-- inoremap ' ''<Left>
-- inoremap " ""<Left>
-- vim.keymap.set("i", "(", "()<Left>")
-- vim.keymap.set("i", "<C-e>", "<Del>")

-- cd to current file directory
vim.keymap.set("n", "<leader>cd", ":cd %:p:h<cr>", { desc = "cd current file dir" })

-- Optional
-- -- Parser info
-- vim.keymap.set("n", "<leader><F2>", ":InspectTree<CR>", { desc = "Inspect Tree" })
-- -- Delete selected text and replace with text from system clipboard
-- vim.keymap.set("x", "<leader>p", [["_dP]], { desc = "Replace selection with system clipboard content" })
--
-- -- Yank selected text to system clipboard
-- vim.keymap.set({ "n", "v" }, "<leader>y", [["+y]], { desc = "Yank to system clipboard" })
--
-- -- Yank current line to system clipboard
-- vim.keymap.set("n", "<leader>Y", [["+Y]], { desc = "Yank current line to system clipboard" })
