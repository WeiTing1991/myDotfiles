---------------------------------------------  GLOBALS SETTINGS -------------------------------------------------------
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
  markdown_recommended_style = 0,
  editorconfig = true,
}

for k, v in pairs(globals) do
  vim.g[k] = v
end

---------------------------------------------- DEFAULT OPTIONS ---------------------------------------------------------

-- undo folder
local undoDir = ""
if is_mac then
  undoDir = os.getenv("HOME") .. "/.vim/undodir"
elseif is_windows then
  undoDir = os.getenv("HOME") .. "\\.vim\\undodir"
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
  completeopt = "menu,menuone,noselect,noinsert",

  --number
  number = true,
  relativenumber = true,
  numberwidth = 4,

  -- indention
  autoindent = true,
  smartindent = false,
  expandtab = true,
  shiftwidth = 2,
  tabstop = 2,
  softtabstop = 2,

  breakindent = true,
  showbreak = "↳ ",
  linebreak = true,

  -- whitespace
  list = true,
  listchars = { tab = "▏ ", trail = "·", lead = "·", extends = "»", precedes = "«" },
  fillchars = {
    foldopen = "",
    foldclose = "",
    fold = " ",
    foldsep = " ",
    diff = "╱",
    eob = " ",
    vert = "┃", -- Heavy vertical
    vertleft = "┫",
    vertright = "┣",
    horiz = "━", -- Heavy horizontal
    horizup = "┻",
    horizdown = "┳",
  },

  -- search
  ignorecase = true,
  smartcase = true,
  incsearch = false,
  hlsearch = true,

  -- Preview substitutions live, as you type!
  shortmess = vim.opt.shortmess + { c = true },
  inccommand = "split",
  wildignore = vim.opt.wildignore + { "*/node_modules/*", "*/.git/*", "*/vendor/*" },

  sessionoptions = "curdir,folds,globals,help,tabpages,terminal,winsize",

  -- Save undo history
  backup = false,
  swapfile = false,
  shadafile = "NONE",
  writebackup = false,
  undofile = true,
  undodir = undoDir,

  -- wrap
  wrap = false,
  colorcolumn = "120",
  textwidth = 110,

  -- searchfolds
  foldmethod = "expr",
  foldexpr = "nvim_treesitter#foldexpr()",
  foldlevel = 99,
  foldtext = "",
  foldenable = true,
  foldnestmax = 3,

  -- ui
  cursorline = false,
  mouse = "a",
  mousemodel = "popup",
  showtabline = 2,
  laststatus = 3,
  ttyfast = true,
  signcolumn = "yes:4",
  winborder = "rounded",

  --paste
  paste = false,
  -- splits
  splitbelow = true,
  splitright = true,

  -- Decrease update time
  updatetime = 50,
  -- Decrease mapped sequence wait time
  timeoutlen = 400,

  -- spelling
  spell = false,
  spelllang = { "en_us", "en" },
  spellfile = spellDir,
}

for k, v in pairs(options) do
  vim.opt[k] = v
end

vim.schedule(function()
  local ok, _ = pcall(function()
    vim.opt.clipboard = "unnamedplus"
  end)
  if not ok then
    print("Clipboard not available")
  end
end)
