local undoDir = ''

if vim.loop.os_uname().sysname == 'Darwin' then
  undoDir = os.getenv 'HOME' .. '/.vim/undodir'
elseif vim.loop.os_uname().sysname == 'windoes' then
  undoDir = os.getenv 'USERPROFILE' .. '/.vim/undodir'
else
  undoDir = os.getenv 'HOME' .. '/.vim/undodir'
end

local options = {
  -- See :help
  number = true,
  relativenumber = true,
  ma = true,
  mouse = 'a',

  --  See `:help 'clipboard'`
  clipboard = 'unnamedplus',

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
  updatetime = 200,
  timeoutlen = 400,

  -- Configure how new splits should be opened
  splitright = true,
  splitbelow = true,

  compatible = false,
  -- Sets how neovim will display certain whitespace in the editor.
  --  See `:help 'list'`
  listchars = { tab = ' . ', trail = ' ', nbsp = '+' },
  list = true,

  -- Preview substitutions live, as you type!
  --inccommand = "split"
  signcolumn = 'yes',

  -- Show which line your cursor is on
  cursorline = true,
  cursorcolumn = false,

  -- Minimal number of screen lines to keep above and below the cursor.
  scrolloff = 40,
  sidescrolloff = 1,
  conceallevel = 0, -- so that `` is visible in markdown files

  -- search
  -- enable incremental searching
  incsearch = true,
  hlsearch = false,

  -- apperance
  termguicolors = true,
  background = 'dark',
  cmdheight = 1,

  -- behavior
  hidden = true,
  errorbells = false,
  swapfile = false,
  backspace = 'indent,eol,start',

  autochdir = false,
  selection = 'exclusive',
  modifiable = true,
  encoding = 'UTF-8',

  -- folds
  foldmethod = 'expr',
  foldexpr = 'nvim_treesitter#foldexpr()',
  foldlevel = 99,

  --titlestring = string.sub('%{&pvw} - %F', 0, 10),
}

for k, v in pairs(options) do
  vim.opt[k] = v
end

--isfname = append("@-@"),
-- iskeyword:append("-"),
