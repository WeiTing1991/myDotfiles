-- globals settings
local globals = {
  mapleader = " ",
  prev_buffer = nil,
  next_buffer = nil,
  have_nerd_font = true,
  -- editorconfig = true,
}

for k, v in pairs(globals) do
  vim.g[k] = v
end

-- options
-- vim.opt.isfname:append "@-@"
-- vim.opt.iskeyword:append "-"

local undoDir = ""
if vim.loop.os_uname().sysname == "Darwin" then
  undoDir = os.getenv "HOME" .. "/.vim/undodir"
elseif vim.fn.has "Win32" then
  undoDir = os.getenv "HOME" .. "/.vim/undodir"
else
  undoDir = os.getenv "HOME" .. "/.vim/undodir"
end

local spelldir = ""
local spell_word = {}
if vim.loop.os_uname().sysname == "Darwin" then
  spelldir = vim.fn.stdpath "config" .. "/spell/en.utf-8.add"
elseif vim.fn.has "Win32" then
  spelldir = os.getenv "HOME" .. "/spell/en.utf-8.add"
else
  spelldir = os.getenv "HOME" .. "/spell/en.utf-8.add"
end

local options = {
  -- See :h
  number = true,
  relativenumber = true,
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
  smartindent = false,
  cindent = false,
  wrap = true,

  -- Save undo history
  backup = false,
  undofile = true,
  undodir = undoDir,

  -- Case-insensitive searching UNLESS \C or capital in search
  ignorecase = true,
  smartcase = false,

  -- Decrease update time
  updatetime = 30,
  timeoutlen = 300,

  -- Configure how new splits should be opened
  splitright = true,
  --splitbelow = true,

  -- Sets how neovim will display certain whitespace in the editor.
  -- See `:help 'list'`
  list = true,
  listchars = { tab = "▏ ", trail = "·", extends = "»", precedes = "«" },
  fillchars = { eob = " " },

  -- Preview substitutions live, as you type!
  inccommand = "split",
  signcolumn = "yes:2",
  -- foldcolumn = "1",

  -- Show which line your cursor is on
  cursorline = true,

  -- colorcolumn = "120",
  textwidth = 120,
  linebreak = true,

  -- Minimal number of screen lines to keep above and below the cursor.
  scrolloff = 10,
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
  foldtext = "",
  foldlevel = 99,
  -- foldexpr = "v:lua.require'lazyvim.util'.ui.foldexpr()",

  -- spell check
  spelllang = "en_us",
  spell = false,
  spellfile = spelldir,
  --titlestring = string.sub('%{&pvw} - %F', 0, 10),
}

for k, v in pairs(options) do
  vim.opt[k] = v
end



