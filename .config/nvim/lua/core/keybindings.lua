----- default keymap -----
local map = vim.keymap.set

-- disable space key and remapping the keys
vim.keymap.set({ "n", "v" }, "<Space>", "<Nop>", { silent = true })
vim.keymap.set({ "n", "v" }, "<F1>", "<Nop>", { silent = true })
vim.keymap.set({ "n", "v" }, "<C-z>", "<Nop>", { silent = true })
vim.keymap.set({ "n", "v" }, "<C-m>", "<C-m>", { silent = true })

-- buffer
vim.keymap.set({ "n", "v" }, "<C-i>", "<C-i>", { noremap = true })
vim.keymap.set({ "n", "v" }, "<C-o>", "<C-o>", { noremap = true  })
vim.keymap.set("n", "<C-i>", ":bp<cr>", { desc = "Prevous buffer" })
vim.keymap.set("n", "<C-o>", ":bn<cr>", { desc = "Next buffer" })

-- vim.keymap.set("n", "<leader>o", ":bnext<cr>", { desc = "Next buffer" })
-- vim.keymap.set("n", "<leader>i", ":bprevious<cr>", { desc = "Prevous buffer" })

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
end, { noremap=true, silent=true, expr=false, desc = "Toggle fold" })

vim.keymap.set("n", "<leader>q", function()
  vim.cmd ":bw"
end, { desc = "Close current buffer and window" })


-- clear search highlights
vim.keymap.set("n", "<Esc>", ":nohl<CR>", { desc = "Clear highlights" })
vim.keymap.set("n", "zz", ":nohl<CR>", { desc = "Clear highlights" })

-- comment
vim.api.nvim_set_keymap("n", "-", "gcc", { desc = "comment" })
vim.api.nvim_set_keymap("v", "-", "gc", { desc = "comment" })

vim.api.nvim_set_keymap("n", "_", "gbc", { desc = "comment blockwise" })
vim.api.nvim_set_keymap("v", "_", "gb", { desc = "comment blockwise" })

-- Leave insert mode
vim.keymap.set("i", "<C-c>", "<Esc>")
vim.keymap.set("i", "<Esc>", "<C-c>")
vim.keymap.set("i", "jk", "<Esc>")
