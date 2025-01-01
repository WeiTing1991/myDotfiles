-- load UI
vim.g.base46_cache = vim.fn.stdpath "data" .. "/base46_cache/"

-- load packages
require "core.config"
require "core.keymaps"
require "core.autocmds"

-- load plugin manager
require "core.lazy"


-- -- for image preview
-- package.path = package.path .. ";" .. vim.fn.expand("$HOME") .. "/.luarocks/share/lua/5.1/?/init.lua"
-- package.path = package.path .. ";" .. vim.fn.expand("$HOME") .. "/.luarocks/share/lua/5.1/?.lua"

-- list of basd46
local packages = {
  "defaults",
  "syntax",
  "statusline",
  "telescope",
  "treesitter",
  "nvcheatsheet",
  "cmp",
  -- "whichkey"
  -- lsp
  -- mason

  -- blankline
  -- colors
  -- devicons
  -- git
  -- nvimtree
  -- tbline
  -- term
}

-- load base46 UI
for _, v in ipairs(packages) do
  dofile(vim.g.base46_cache .. v)
end

-- load base46 UI
-- for _, v in ipairs(vim.fn.readdir(vim.g.base46_cache)) do
--   dofile(vim.g.base46_cache .. v)
-- end

