------------------------------------ GLOBALS SETTINGS ------------------------------------

local globals = {

  prev_buffer = nil,
  next_buffer = nil,
  have_nerd_font = true,

  -- disable some default providers
  loaded_node_provider = 0,
  loaded_python3_provider = 0,
  loaded_perl_provider = 0,
  loaded_ruby_provider = 0,
}

for k, v in pairs(globals) do
  vim.g[k] = v
end

------------------------------------ DEFAULT OPTIONS ------------------------------------

-- undo folder
local undoDir = ""
if vim.loop.os_uname().sysname == "Darwin" then
  undoDir = os.getenv "HOME" .. "/.vim/undodir"
elseif vim.fn.has "Win32" then
  undoDir = os.getenv "HOME" .. "/.vim/undodir"
else
  undoDir = os.getenv "HOME" .. "/.vim/undodir"
end

-- spell folder
local spellDir = ""

local spell_word = {}
if vim.loop.os_uname().sysname == "Darwin" then
  spellDir = vim.fn.stdpath "config" .. "/spell/en.utf-8.add"
elseif vim.fn.has "Win32" then
  spellDir = os.getenv "HOME" .. "/spell/en.utf-8.add"
else
  spellDir = os.getenv "HOME" .. "/spell/en.utf-8.add"
end


local options = {

  -- misc
  backspace = { 'eol', 'start', 'indent' },
  encoding = 'utf-8',
  matchpairs = { '(:)', '{:}', '[:]', '<:>' },
  syntax = 'enable',


  -- indention
  autoindent = true,
  expandtab = true,
  shiftwidth = 2,
  smartindent = true,
  tabstop = 2,
  softtabstop = 2,

  --number
  number = true,
  relativenumber = true,
  numberwidth = 2,

  -- ui
  mouse = "a",
  cursorline = true,
  laststatus = 3,
  -- cursorlineopt = "",

  clipboard = "unnamedplus",

  list = true,
  listchars = { tab = "▏ ", trail = "·", lead = "·", extends = "»", precedes = "«" },
  fillchars = { eob = " " },
  wildmenu = true,
  signcolumn = "yes:1",
  splitright = true,
  splitbelow = true,

  breakindent = true,
  showbreak = "↪ ",

  wrap = false,
  -- cindent = false,
  -- showtabline = 1,

  -- Case-insensitive searching UNLESS \C or capital in search
  -- search
  hlsearch = true,
  ignorecase = true,
  smartcase = true,
  wildignore = vim.opt.wildignore + { '*/node_modules/*', '*/.git/*', '*/vendor/*' },


  -- Save undo history
  backup = false,
  swapfile = false,
  writebackup  = false,
  undofile = true,
  undodir = undoDir,

  -- Preview substitutions live, as you type!
  inccommand = "split",
  shortmess = vim.opt.shortmess + { c = true },

  -- colorcolumn = "120",
  textwidth = 110, -- virt column set to 110
  linebreak = true,

  -- Minimal number of screen lines to keep above and below the cursor.
  scrolloff = 10,
  -- sidescrolloff = 1,
  -- conceallevel = 0,

  -- searchfolds
  foldmethod = "expr",
  foldexpr = "nvim_treesitter#foldexpr()",
  -- foldexpr = "v:lua.require'lazyvim.util'.ui.foldexpr()",
  foldtext = "",
  foldlevel = 99,
  foldnestmax = 4,
  -- foldcolumn = "0",
  -- foldlevelstart = 99,

  --performace
  updatetime = 50,
  redrawtime = 1000,
  timeoutlen = 200,
  ttimeoutlen = 10,

  -- spell check
  spelllang = "en_us",
  spell = false,
  spellfile = spellDir,

  termguicolors = true,
}

-- vim.opt.isfname:append "@-@"
-- vim.opt.iskeyword:append "-"

for k, v in pairs(options) do
  vim.opt[k] = v
end
