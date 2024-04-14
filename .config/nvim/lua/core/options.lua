local undoDir = ""

if vim.loop.os_uname().sysname == "Darwin" then
  undoDir = os.getenv "HOME" .. "/.vim/undodir"
elseif vim.fn.has "Win32" then
  undoDir = os.getenv "USERPROFILE" .. "\\.vim\\undodir"
else
  undoDir = os.getenv "HOME" .. "/.vim/undodir"
end

local spelldir = ""

if vim.loop.os_uname().sysname == "Darwin" then
  spelldir = vim.fn.stdpath "config" .. "/spell/en.utf-8.add"
elseif vim.fn.has "Win32" then
  spelldir = os.getenv "USERPROFILE" .. "/spell/en.utf-8.add"
else
  spelldir = os.getenv "HOME" .. "/spell/en.utf-8.add"
end

local options = {
  -- See :help
  number = true,
  relativenumber = true,
  ma = true,
  mouse = "a",

  --  See `:help 'clipboard'`
  clipboard = "unnamedplus",

  -- Enable break indent
  breakindent = false,
  -- set tab to 2 spaces
  tabstop = 4,
  softtabstop = 4,
  shiftwidth = 4,
  expandtab = true,
  smartindent = true,
  wrap = false,

  -- Save undo history
  backup = false,
  undofile = true,
  undodir = undoDir,

  -- Case-insensitive searching UNLESS \C or capital in search
  ignorecase = true,
  smartcase = false,

  -- Decrease update time
  updatetime = 50,
  timeoutlen = 400,

  -- Configure how new splits should be opened
  splitright = true,
  splitbelow = true,

  compatible = false,
  -- Sets how neovim will display certain whitespace in the editor.
  --  See `:help 'list'`
  listchars = { tab = "· ", trail = "·", extends="»",precedes="«"},
  list = true,

  -- Preview substitutions live, as you type!
  --inccommand = "split"
  signcolumn = "yes",
  -- Show which line your cursor is on
  cursorline = true,
  colorcolumn = "120",
  textwidth = 120,
  -- Minimal number of screen lines to keep above and below the cursor.
  scrolloff = 40,
  sidescrolloff = 1,
  conceallevel = 0,

  -- search
  -- enable incremental searching
  incsearch = true,
  hlsearch = false,

  -- apperance
  termguicolors = true,
  background = "dark",
  cmdheight = 1,

  -- behavior
  hidden = true,
  errorbells = false,
  swapfile = false,
  backspace = "indent,eol,start",

  autochdir = false,
  selection = "exclusive",
  modifiable = true,
  encoding = "UTF-8",

  -- folds
  foldmethod = "expr",
  foldexpr = "nvim_treesitter#foldexpr()",
  foldlevel = 99,

  -- spell check
  spelllang = "en_us",
  spell = false,
  spellfile = spelldir,
  --titlestring = string.sub('%{&pvw} - %F', 0, 10),
}

for k, v in pairs(options) do
  vim.opt[k] = v
end

vim.opt.isfname:append("@-@")
vim.opt.iskeyword:append("-")
