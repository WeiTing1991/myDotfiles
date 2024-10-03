vim.g.base46_cache = vim.fn.stdpath "data" .. "/base46_cache/"

require("core.globals")
require("core.options")
require("core.keymaps")
require("core.autocmds")
require("core.lazy")

 -- put this after lazy setup
dofile(vim.g.base46_cache .. "defaults")
dofile(vim.g.base46_cache .. "statusline")

-- To load all integrations at once
for _, v in ipairs(vim.fn.readdir(vim.g.base46_cache)) do
  dofile(vim.g.base46_cache .. v)
end

vim.opt.guicursor = "n-v-c:block-Cursor,n-v-c-i:blinkon1,i:ver1000-Cursor,r-cr-o:hor100-cursor"
vim.api.nvim_set_hl(0, "cursor", { background = "#eb6f92", foreground = "white"})
