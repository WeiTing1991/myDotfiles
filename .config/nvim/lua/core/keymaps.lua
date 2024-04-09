--vim.keymap.set("n", keys, func, { buffer = event.buf, desc = "LSP: " .. desc })

function ToggleSpellCheck()
  if vim.o.spell then
    vim.o.spell = false
    print "Spellcheck OFF"
  else
    vim.o.spell = true
    print "Spellcheck ON"
  end
end

-- cd to current file directory
vim.keymap.set("n", "<leader>cd", ":cd %:p:h", { desc = "cd current file dir" })

vim.keymap.set("n", "<leader>G", "<cmd>LazyGit<cr>", { desc = "open lazygit" })
vim.keymap.set("n", "<leader>u", vim.cmd.UndotreeToggle, { desc = "Undotree" })

-- markdown preview
vim.keymap.set("n", "<leader>mk", "<cmd>MarkdownPreviewToggle<cr>", { desc = "markdown toggle" })
vim.keymap.set("n", "<leader>mkp", "<cmd>MarkdownPreview<cr>", { desc = "markdown preview" })
vim.keymap.set("n", "<leader>mks", "<cmd>MarkdownPreviewStop<cr>", { desc = "markdown stop" })

-- spell check toggle
vim.api.nvim_set_keymap("n", "<Leader>sp", ":lua ToggleSpellCheck()<CR>", { desc = "spell check off" })

--leave the file
vim.keymap.set("n", "<leader>q", function()
  vim.cmd ":bw"
end, { desc = "Close windows" })

vim.keymap.set("n", "<C-s>", function()
  vim.cmd ":w"
end, { desc = "Save file" })

vim.keymap.set("n", "<C-a>", "ggVG" , { desc = "select all" })

-- Parser info
vim.keymap.set("n", "<leader><F2>", ":InspectTree<CR>", { desc = "Inspect Tree" })

-- clear search highlights
vim.keymap.set("n", "<leader>zz", ":nohl<CR>", { desc = "Clear highlights" })

-- open the tree
--vim.keymap.set("n", "<leader>e", ":25Lex<cr>")
-- disable the space key
vim.keymap.set({ "n", "v" }, "<Space>", "<Nop>", { silent = true })

-- autopair
vim.keymap.set('i', '""', '""<left>', { desc = '' })
vim.keymap.set('i', "''", "''<left>", { desc = '' })
vim.keymap.set('i', '()', '()<left>', { desc = '' })
vim.keymap.set('i', '{}', '{}<left>', { desc = '' })
vim.keymap.set('i', '[]', '[]<left>', { desc = '' })

-- split the windows
--vim.keymap.set("n", "<leader>sv", "<C-w>v", { desc = "split window vertically" })
--vim.keymap.set("n", "<leader>sh", "<C-w>s", { desc = "split window horizontally" })
--vim.keymap.set("n", "<leader>se", "<C-w>=", { desc = "equa all window " })
--vim.keymap.set("n", "<leader>sx", ":close<CR>", { desc = "close current window" })

-- Remove the old global variables
-- move between buffer
if vim.loop.os_uname().sysname == "Darwin" then
  vim.keymap.set("n", "<C-o>", ":bn<cr>", { desc = "Next buffer" })
  vim.keymap.set("n", "<C-i>", ":bp<cr>", { desc = "Prevous buffer" })
elseif vim.fn.has "Win32" then
  vim.keymap.set("n", "<leader>o", ":bn<cr>", { desc = "Next buffer" })
  vim.keymap.set("n", "<leader>i", ":bp<cr>", { desc = "Prevous buffer" })
  vim.keymap.set("n", "<C-o>", ":nohl<CR>", { desc = "Next buffer" })
  vim.keymap.set("n", "<C-i>", ":nohl<CR>", { desc = "Prevous buffer" })
else
  vim.keymap.set("n", "<C-o>", ":bn<cr>", { desc = "Next buffer" })
  vim.keymap.set("n", "<C-i>", ":bp<cr>", { desc = "Prevous buffer" })
end

-- indenting
vim.keymap.set("v", "<", "<gv")
vim.keymap.set("v", ">", ">gv")

-- comments
vim.api.nvim_set_keymap("n", "<C-_>", "gcc", { desc = "comment" })
vim.api.nvim_set_keymap("v", "<C-_>", "gc", { desc = "comment" })

-- Move the highlighted line down
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv", { desc = "Move selection down" })
-- Move the highlighted line up
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv", { desc = "Move selection up" })

-- Join current line with the line below
vim.keymap.set("n", "J", "mzJ`z", { desc = "Join lines" })

-- Keep cursor centered when scrolling
vim.keymap.set("n", "<C-d>", "<C-d>zz", { desc = "Scroll half page down" })
vim.keymap.set("n", "<C-u>", "<C-u>zz", { desc = "Scroll half page up" })

-- Keep cursor centered when jumping with 'n' and 'N'
vim.keymap.set("n", "n", "nzzzv", { desc = "Jump to next match" })
vim.keymap.set("n", "N", "Nzzzv", { desc = "Jump to previous match" })

-- Delete selected text and replace with text from system clipboard
vim.keymap.set("x", "<leader>p", [["_dP]], { desc = "Replace selection with system clipboard content" })
--vim.keymap.set("v", "<leader>p", "\"_dP", {desc = "great remap paste"})

-- Yank selected text to system clipboard
vim.keymap.set({ "n", "v" }, "<leader>y", [["+y]], { desc = "Yank to system clipboard" })

-- Yank current line to system clipboard
vim.keymap.set("n", "<leader>Y", [["+Y]], { desc = "Yank current line to system clipboard" })

-- Replace the word throughout the file
vim.keymap.set(
  "n",
  "<leader>ss",
  [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]],
  { desc = "Replace word throughout file" }
)

-- Leave insert mode by pressing leader followed by backspace
vim.keymap.set("i", "qq", "<Esc>")
vim.keymap.set("n", "<Tab>", "<Nop>")
