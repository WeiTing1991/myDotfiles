------------------------------------ GLOBALS SETTINGS ------------------------------------

-- add binaries installed by mason.nvim to path
local is_windows = vim.loop.os_uname().sysname == "Windows_NT"
local is_mac = vim.loop.os_uname().sysname == "Darwin"

local sep = is_windows and "\\" or "/"
local delim = is_windows and ";" or ":"
vim.env.PATH = table.concat({ vim.fn.stdpath("data"), "mason", "bin" }, sep) .. delim .. vim.env.PATH

local globals = {

  prev_buffer = nil,
  next_buffer = nil,
  have_nerd_font = true,

  -- disable some default providers
  loaded_node_provider = 0,
  loaded_python3_provider = 0,
  loaded_perl_provider = 0,
  loaded_ruby_provider = 0,

  -- disable auto format
  autoformat = false,
}

for k, v in pairs(globals) do
  vim.g[k] = v
end

------------------------------------ DEFAULT OPTIONS ------------------------------------

-- undo folder
local undoDir = ""
if is_mac then
  undoDir = os.getenv("HOME") .. "/.vim/undodir"
elseif is_windows then
  undoDir = vim.fn.stdpath("config") .. "\\.vim\\undodir"
else
  undoDir = os.getenv("HOME") .. "/.vim/undodir"
end

-- spell folder
local spellDir = ""
local spell_word = {}
if is_mac then
  spellDir = vim.fn.stdpath("config") .. "/spell/en.utf-8.add"
elseif is_windows then
  spellDir = vim.fn.stdpath("config") .. "\\spell\\en.utf-8.add"
else
  spellDir = vim.fn.stdpath("config") .. "/spell/en.utf-8.add"
end

local options = {
  backspace = { "eol", "start", "indent" },
  encoding = "utf-8",
  matchpairs = { "(:)", "{:}", "[:]", "<:>" },
  syntax = "enable",
  clipboard = "unnamedplus",
  completeopt = "menu,menuone,noselect,noinsert",

  -- indention
  autoindent = true,
  expandtab = true,
  smartindent = true,
  shiftwidth = 2,
  tabstop = 2,
  softtabstop = 2,

  --number
  number = true,
  relativenumber = true,
  numberwidth = 4,

  -- ui
  mouse = "a",
  mousescroll = "ver:3,hor:0",
  cursorline = true,
  laststatus = 3,
  termguicolors = true,
  winborder = "rounded",
  -- cursorlineopt = "",

  list = true,
  listchars = { tab = "▏ ", trail = "·", lead = "·", extends = "»", precedes = "«" },
  fillchars = { eob = " " },
  wildmenu = true,
  signcolumn = "yes:1",
  -- split options
  splitright = true,
  splitbelow = true,

  breakindent = true,
  showbreak = "↪ ",

  wrap = true,
  -- cindent = false,
  -- showtabline = 1,

  -- Case-insensitive searching UNLESS \C or capital in search
  -- search
  incsearch = false,
  -- Preview substitutions live, as you type!
  shortmess = vim.opt.shortmess + { c = true },
  inccommand = "split",
  ignorecase = true,
  smartcase = true,
  hlsearch = true,
  wildignore = vim.opt.wildignore + { "*/node_modules/*", "*/.git/*", "*/vendor/*" },

  -- Save undo history
  backup = false,
  swapfile = false,
  writebackup = false,
  undofile = true,
  undodir = undoDir,

  -- colorcolumn = "120",
  textwidth = 110, -- virt column set to 110
  linebreak = true,

  -- Minimal number of screen lines to keep above and below the cursor.
  scrolloff = 20,
  sidescrolloff = 0,
  -- conceallevel = 0,

  -- searchfolds
  foldmethod = "expr",
  foldexpr = "nvim_treesitter#foldexpr()",
  -- foldexpr = "v:lua.require'lazyvim.util'.ui.foldexpr()",
  foldtext = "",
  foldlevel = 99,
  foldnestmax = 4,
  foldlevelstart = 99,
  foldenable = true,

  --performace
  updatetime = 30,
  -- redrawtime = 100,
  timeoutlen = 200,
  ttimeoutlen = 100,

  -- spell check
  spelllang = "en_us",
  spell = false,
  spellfile = spellDir,
}

vim.opt.isfname:append "@-@"
-- vim.opt.iskeyword:append "-"
vim.opt.whichwrap:append("<>[]hl")

for k, v in pairs(options) do
  vim.opt[k] = v
end

-- print("is_windows: ", is_windows)
-- print("is_mac: ", is_mac)
