----------------------------------- GLOBALS SETTINGS ------------------------------------

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

local options = {
  --number
  number = true,
  relativenumber = true,
  numberwidth = 4,

  -- indent
  breakindent = true,
  expandtab = true,
  shiftwidth = 2,
  tabstop = 2,
  softtabstop = 2,

  -- whitespace
  list = true,
  listchars = { tab = "▏ ", trail = "·", lead = "·", extends = "»", precedes = "«" },

  -- Case-insensitive
  ignorecase = true,
  smartcase = true,

  -- Decrease update time
  updatetime = 150,
  -- Decrease mapped sequence wait time
  timeoutlen = 250,

  -- Save undo history
  backup = false,
  swapfile = false,
  writebackup = false,

  splitright = true,
  splitbelow = true,

  cursorline = true,
  confirm = true,

  -- mouse
  mouse = "a",
}

for k, v in pairs(options) do
  vim.opt[k] = v
end

vim.schedule(function()
  vim.o.clipboard = 'unnamedplus'
end)
