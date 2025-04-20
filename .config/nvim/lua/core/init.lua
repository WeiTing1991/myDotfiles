vim.cmd "colorscheme retrobox"

require "core.options"
require "core.autocmds"
require "core.keybindings"

vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- Plugin manager
-- bootstrap lazy and all plugins
local lazypath = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"

if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system { "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath }
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end

vim.opt.rtp:prepend(lazypath)

local lazy_config = require "core.lazy"

-- load plugins
require("lazy").setup({
  { import = "plugin.init" },
  { import = "plugin.editor" },
  { import = "plugin.lsp" },
  { import = "plugin.ui" },
  { import = "plugin.git" },
  { import = "plugin.tools" },

  { import = "plugin.dap" },
  --  { import = "plugin.note_taking" },
  --  { import = "plugin.misc" },
}, lazy_config)

require "keymappings"
