-- See `:help vim.opt`
--  For more options, you can see `:help option-list`

vim.opt.number = true
vim.opt.relativenumber = true

-- Enable mouse mode, can be useful for resizing splits for example!
vim.opt.mouse = "a"

-- Don't show the mode, since it's already in status line
vim.opt.showmode = false

-- Sync clipboard between OS and Neovim.
--  Remove this option if you want your OS clipboard to remain independent.
--  See `:help 'clipboard'`
vim.opt.clipboard = "unnamedplus"

-- Enable break indent
vim.opt.breakindent = true
-- set tab to 2 spaces
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.smartindent = true
vim.opt.wrap = false

-- Save undo history
vim.opt.backup = false
vim.opt.undofile = true
vim.opt.undodir = os.getenv("HOME") .. "/.vim/undodir"

-- Case-insensitive searching UNLESS \C or capital in search
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Keep signcolumn on by default
vim.opt.signcolumn = "no"

-- Decrease update time
vim.opt.updatetime = 250
vim.opt.timeoutlen = 300

-- Configure how new splits should be opened

vim.opt.splitright = true
vim.opt.splitbelow = true

-- Sets how neovim will display certain whitespace in the editor.
--  See `:help 'list'`
--  and `:help 'listchars'`
vim.opt.listchars = { tab = " .", trail = ".", nbsp = "+" }
vim.opt.list = true

-- Preview substitutions live, as you type!
vim.opt.inccommand = "split"

-- Show which line your cursor is on
vim.opt.cursorline = true
vim.opt.cursorcolumn = true

-- Minimal number of screen lines to keep above and below the cursor.
vim.opt.scrolloff = 40
vim.opt.sidescrolloff = 1

vim.opt.conceallevel = 0 -- so that `` is visible in markdown files

-- search
vim.opt.incsearch = true
vim.opt.hlsearch = false

-- apperance
vim.opt.termguicolors = true
vim.opt.background = "dark"
vim.opt.isfname:append("@-@")
vim.opt.cmdheight = 1
vim.opt.colorcolumn = "120"

-- behavior
vim.opt.hidden = true
vim.opt.errorbells = false
vim.opt.swapfile = false
vim.opt.backspace = "indent,eol,start"

vim.opt.autochdir = false
vim.opt.iskeyword:append("-")
vim.opt.selection = "exclusive"
vim.opt.modifiable = true
vim.opt.encoding = "UTF-8"

-- folds
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
vim.opt.foldlevel = 99

vim.opt.title = true
vim.opt.titlelen = 0 -- do not shorten title
-- vim.g.netrw_banner = 1
-- vim.g.nertw_liststyle = 3
--vim.g.netrw_mouse = 2
