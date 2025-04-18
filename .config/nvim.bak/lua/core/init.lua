-- init
vim.g.base46_cache = vim.fn.stdpath "data" .. "/base46_cache/"
vim.g.mapleader = " "
vim.g.maplocalleader = ','

-- default
require "core.options"
require "core.keymappings"
require "core.autocmds"

local lazypath = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"

-- Plugin manager
-- bootstrap lazy and all plugins
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system { "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath }
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out,                            "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

local lazy_config = require("core.lazy")

-- load plugins
require("lazy").setup({
  { import = "plugin.init" },
  { import = "plugin.lsp" },
  { import = "plugin.editor" },
  { import = "plugin.tools" },
  { import = "plugin.note_taking" },
  { import = "plugin.misc" },
}, lazy_config)

-- loading list from basd46

local packages = {
  "defaults",
  "telescope",
  -- "statusline",
  "devicons",
  "colors",
  "syntax",
  "treesitter",
  "nvcheatsheet",
  "nvimtree",
  "blankline",
  "mason",
  "cmp",
  "lsp",
  "git",
  -- "diffview",
  -- "tbline",
  -- "term",
  -- "whichkey"
}


-- load base46 UI
-- individual files
for _, v in ipairs(packages) do
  local file_path = vim.g.base46_cache .. v
  if vim.loop.fs_stat(file_path) then
    dofile(file_path)
  else
    vim.notify("Base46: Missing file " .. file_path, vim.log.levels.WARN)
  end
end

-- load all
-- for _, v in ipairs(vim.fn.readdir(vim.g.base46_cache)) do
--   dofile(vim.g.base46_cache .. v)
-- end

-- custom mapping
require("mapping")
require("lsp_keymaps")
