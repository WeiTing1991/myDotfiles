-- ------------------------- default keymap -------------------------------------

-- disable the key
vim.keymap.set({ "n", "v" }, "<Space>", "<Nop>", { silent = true })

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
vim.keymap.set("n", "<leader>o", ":bn<cr>", { desc = "Next buffer" })
vim.keymap.set("n", "<leader>i", ":bp<cr>", { desc = "Prevous buffer" })

vim.keymap.set("n", "<leader>q", function()
  vim.cmd ":bw"
end, { desc = "Close current buffer and window" })

-- clear search highlights
vim.keymap.set("n", "zz", ":nohl<CR>", { desc = "Clear highlights" })

-- Move the highlighted line down
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv", { desc = "Move selection down" })
-- Move the highlighted line up
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv", { desc = "Move selection up" })

-- Join current line with the line below
vim.keymap.set("n", "J", "mzJ`z", { desc = "Join lines" })

-- Keep cursor centered when scrolling
vim.keymap.set("n", "<C-d>", "<C-d>zz", { desc = "Scroll half page down" })
vim.keymap.set("n", "<C-u>", "<C-u>zz", { desc = "Scroll half page up" })

vim.keymap.set("n", "G", "Gzz", { desc = "Scroll half page up" })

-- Keep cursor centered when jumping with 'n' and 'N'
vim.keymap.set("n", "n", "nzzzv", { desc = "Jump to next match" })
vim.keymap.set("n", "N", "Nzzzv", { desc = "Jump to previous match" })

-- move between neovim buffers
vim.keymap.set("n", "<leader>h", "<C-w>h", { desc = "move to left buffer" })
vim.keymap.set("n", "<leader>l", "<C-w>l", { desc = "move to right buffer" })
vim.keymap.set("n", "<leader>k", "<C-w>k", { desc = "move to up buffer" })
vim.keymap.set("n", "<leader>j", "<C-w>j", { desc = "move to down buffer" })

-- comment
vim.api.nvim_set_keymap("n", "-", "gcc", { desc = "comment" })
vim.api.nvim_set_keymap("v", "-", "gc", { desc = "comment" })

-- open term
vim.keymap.set({ "n", "t" }, "<C-/>", function()
  require("nvchad.term").toggle { pos = "float", id = "floatTerm" }
end, { desc = "open the term" })

-- Leave insert mode by pressing leader followed by backspace
vim.keymap.set("i", "<C-c>", "<Esc>")

-- windows
vim.keymap.set( "n", "<C-'>", function ()
    require("custom_plugins.toggle_maximize_window").toggle_maximize_window()
  end, { desc = "Toggle maximize window" })




