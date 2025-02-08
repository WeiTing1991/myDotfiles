-- init
vim.g.base46_cache = vim.fn.stdpath "data" .. "/base46/"
vim.g.mapleader = " "

-- default
require "wtvim.core.options"
require "wtvim.core.keymappings"

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

local lazy_config = require("wtvim.core.lazy")

-- load plugins
require("lazy").setup({
  { import = "plugins" },
}, lazy_config)


-- -- loading list from basd46
-- local packages = {
--   "defaults",
--   "telescope",
--   "statusline",
--   "devicons",
--   "colors",
--   "syntax",
--   "treesitter",
--   "nvcheatsheet",
--   "nvimtree",
--   "todonvim",
--   "mason",
--   "cmp",
--   "lsp",
--   -- "diffview",
--   -- "lspsaga",
--   -- "git",
--   -- "tbline",
--   -- "term",
--   -- "whichkey"
--   -- "blankline"
-- }


-- -- load base46 UI
-- -- for _, v in ipairs(packages) do
-- --   dofile(vim.g.base46_cache .. v)
-- -- end

-- for _, v in ipairs(vim.fn.readdir(vim.g.base46_cache)) do
--   dofile(vim.g.base46_cache .. v)
-- end


-- -- custom mapping
-- require("mapping")
-- require("core.autocmds")
