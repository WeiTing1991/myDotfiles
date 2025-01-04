-- load UI
vim.g.base46_cache = vim.fn.stdpath "data" .. "/base46_cache/"

-- load packages
require "core.config"
require "core.keymaps"
require "core.autocmds"

-- load plugin manager
require "core.lazy"

-- list of basd46
local packages = {
  "defaults",
  "syntax",
  "telescope",
  "treesitter",
  -- "cmp",
  -- "lsp",
  -- "mason",

  -- "git",
  -- "statusline",
  -- "nvcheatsheet",
  -- "whichkey"
  -- blankline
  -- colors
  -- devicons
  -- nvimtree
  -- tbline
  -- term
}

-- load base46 UI
for _, v in ipairs(packages) do
  dofile(vim.g.base46_cache .. v)
end
-- load basd46 UI all
-- for _, v in ipairs(vim.fn.readdir(vim.g.base46_cache)) do
--   dofile(vim.g.base46_cache .. v)
-- end

