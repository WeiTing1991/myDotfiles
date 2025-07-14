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
  completeopt = "menu,menuone,noselect,noinsert",

  --number
  number = true,
  relativenumber = true,
  numberwidth = 4,

  -- indention
  autoindent = true,
  smartindent = true,
  expandtab = true,
  shiftwidth = 2,
  tabstop = 2,
  softtabstop = 2,

  breakindent = true,
  showbreak = "↪ ",
  linebreak = true,

  -- whitespace
  list = true,
  listchars = { tab = "▏ ", trail = "·", lead = "·", extends = "»", precedes = "«" },
  fillchars = { eob = " " },
  wildmenu = true,
  signcolumn = "yes:1",

  -- split options
  splitright = true,
  splitbelow = true,

  -- search
  ignorecase = true,
  smartcase = true,
  incsearch = false,
  -- Preview substitutions live, as you type!
  shortmess = vim.opt.shortmess + { c = true },
  inccommand = "split",
  hlsearch = true,
  wildignore = vim.opt.wildignore + { "*/node_modules/*", "*/.git/*", "*/vendor/*" },

  -- Decrease update time
  updatetime = 100,
  -- Decrease mapped sequence wait time
  timeoutlen = 200,

 -- Save undo history
  backup = false,
  swapfile = false,
  writebackup = false,
  undofile = true,
  undodir = undoDir,

  -- wrap
  wrap = true,
  colorcolumn = "120",
  textwidth = 110,

  -- searchfolds
  foldmethod = "expr",
  foldexpr = "nvim_treesitter#foldexpr()",
  foldtext = "",
  foldlevel = 99,
  foldnestmax = 4,
  foldlevelstart = 99,
  foldenable = true,

  -- ui
  cursorline = true,
  confirm = true,
  mouse = "a",
  --mousescroll = "ver:3,hor:0",
  cursorline = true,
  laststatus = 3,
  termguicolors = true,
  --winborder = "rounded",
}

for k, v in pairs(options) do
  vim.opt[k] = v
end

vim.schedule(function()
  vim.o.clipboard = 'unnamedplus'
end)
