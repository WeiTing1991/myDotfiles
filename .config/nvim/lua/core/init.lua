
-- load UI
vim.g.base46_cache = vim.fn.stdpath "data" .. "/base46_cache/"

require("core.config")
require("core.keymaps")
require("core.autocmds")

-- load plugin manager
require("core.lazy")

for _, v in ipairs(vim.fn.readdir(vim.g.base46_cache)) do
  dofile(vim.g.base46_cache .. v)
end
--
-- -- for image preview
-- package.path = package.path .. ";" .. vim.fn.expand("$HOME") .. "/.luarocks/share/lua/5.1/?/init.lua"
-- package.path = package.path .. ";" .. vim.fn.expand("$HOME") .. "/.luarocks/share/lua/5.1/?.lua"
--
-- list of basd46
-- dofile(vim.g.base46_cache .. "defaults")
-- blankline
-- cmp
-- colors
-- defaults
-- devicons
-- git
-- lsp
-- mason
-- nvcheatsheet
-- nvimtree
-- statusline
-- syntax
-- tbline
-- telescope
-- term
-- treesitter
-- whichkey
