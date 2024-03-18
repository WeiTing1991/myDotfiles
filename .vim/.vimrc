" Line numbers
set number
set relativenumber

" Enable mouse
set mouse=a

" Clipboard settings
set clipboard=unnamedplus

" Indent settings
set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab
set smartindent
set nowrap

" Undo settings
set nobackup
set undofile

" Searching settings
set ignorecase
set smartcase

" Update settings
set updatetime=100
set timeoutlen=200

" Split settings
set splitright
set splitbelow

" Display settings
set list
set listchars=tab:.\ ,trail: ,nbsp:+
set signcolumn=yes
set cursorline

" Scroll settings
set scrolloff=40
set sidescrolloff=1
set conceallevel=0

" Search settings
set incsearch
set nohlsearch

" Appearance settings
set termguicolors
set background=dark
set cmdheight=1

" Behavior settings
set hidden
set noerrorbells
set noswapfile
set backspace=indent,eol,start
set autochdir
set selection=exclusive
set modifiable
set encoding=UTF-8

" Fold settings
" set foldmethod=expr
" set foldexpr=nvim_treesitter#foldexpr()
" set foldlevel=99

let mapleader = " " 
" keyboard settings
vim.keymap.set("n", "<leader>zz", ":nohl<CR>", { desc = "Clear highlights" })

-- Disable the space key
vim.keymap.set({ "n", "v" }, "<Space>", "<Nop>", { silent = true })

-- Move between buffers
vim.keymap.set("n", "<C-i>", "<cmd>bnext<cr>", { desc = "Next buffer" })
vim.keymap.set("n", "<C-o>", "<cmd>bprevious<cr>", { desc = "Previous buffer" })

-- Auto-pair
vim.keymap.set("i", '""', '""<left>', { desc = "" })
vim.keymap.set("i", "''", "''<left>", { desc = "" })
vim.keymap.set("i", "()", "()<left>", { desc = "" })
vim.keymap.set("i", "{}", "{}<left>", { desc = "" })
vim.keymap.set("i", "[]", "[]<left>", { desc = "" })

-- Indenting
vim.keymap.set("v", "<", "<gv")
vim.keymap.set("v", ">", ">gv")

-- Comments
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
vim.keymap.set("i", "jj", "<Esc>")
vim.keymap.set("i", "kk", "<Esc>")
