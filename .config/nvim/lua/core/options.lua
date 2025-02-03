------------------------------------ GLOBALS SETTINGS ------------------------------------

local globals = {
  mapleader = " ",
  prev_buffer = nil,
  next_buffer = nil,
  have_nerd_font = true,

  -- disable some default providers
  loaded_node_provider = 0,
  loaded_python3_provider = 0,
  loaded_perl_provider = 0,
  loaded_ruby_provider = 0,

  -- netrw
  -- netrw_browse_split = 4,
  -- netrw_liststyle = 3,
  -- netrw_winsize = 30,
  -- editorconfig = true,
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

  laststatus = 3,

  --number
  number = true,
  relativenumber = true,
  numberwidth = 2,

  -- Show which line your cursor is on
  cursorline = true,
  -- cursorlineopt = "",

  --  See `:help 'clipboard'`
  clipboard = "unnamedplus",

  -- indenting
  expandtab = true,
  shiftwidth = 2,
  smartindent = true,
  tabstop = 2,
  softtabstop = 2,

  -- See `:help 'list'`
  list = true,
  listchars = { tab = "▏ ", trail = "·", extends = "»", precedes = "«" },
  fillchars = { eob = " " },

  -- Case-insensitive searching UNLESS \C or capital in search
  ignorecase = true,
  smartcase = true,
  mouse = "a",

  signcolumn = "yes:1",

  -- Decrease update time
  updatetime = 10,
  timeoutlen = 150,
  splitright = true,
  splitbelow = true,

  -- Save undo history
  backup = false,
  undofile = true,
  undodir = undoDir,


  -- Enable break indent
  breakindent = true,
  showbreak = "↪ ",

  -- Preview substitutions live, as you type!
  inccommand = "split",

  -- cindent = false,
  -- wrap = true,

  -- tabline
  -- showtabline = 1,

  -- colorcolumn = "120",
  textwidth = 105, -- virt column set to 105
  linebreak = true,

  -- Minimal number of screen lines to keep above and below the cursor.
  -- scrolloff = 10,
  -- sidescrolloff = 1,
  -- conceallevel = 0,

  -- searcfolds
  foldmethod = "expr",
  foldexpr = "nvim_treesitter#foldexpr()",
  -- foldexpr = "v:lua.require'lazyvim.util'.ui.foldexpr()",
  foldtext = "",
  foldlevel = 99,
  -- foldcolumn = "0",
  -- foldlevelstart = 99,
  -- foldnextmax = 3,

  -- spell check
  spelllang = "en_us",
  spell = false,
  spellfile = spellDir,
  -- titlestring = string.sub('%{&pvw} - %F', 0, 10),
}

-- vim.opt.isfname:append "@-@"
-- vim.opt.iskeyword:append "-"

-- disable nvim intro
-- vim.opt.shortmess:append "sI"

for k, v in pairs(options) do
  vim.opt[k] = v
end
