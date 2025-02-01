-- load UI
vim.g.base46_cache = vim.fn.stdpath "data" .. "/base46_cache/"

-- load packages
require "core.config"
require "core.keymaps"
require "core.autocmds"

-- load plugin manager
-- require "core.lazy"
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

require("lazy").setup {
  spec = {
    { import = "plugins" },
  },
  rocks = {
    hererocks = true, -- recommended if you do not have global installation of Lua 5.1.
  },
  ui = {
    border = "rounded",
  },
  defaults = {
    lazy = false,
  },
  change_detection = {
    enabled = true,
    notify = false, -- get a notification when changes are found
  },
  checker = {
    enabled = false,
    notify = true,
  },
  performance = {
    cache = {
      enabled = true,
    },
    reset_packpath = true, -- reset the package path to improve startup time
    rtp = {
      reset = true, -- reset the runtime path to $VIMRUNTIME and your config directory
      ---@type string[]
      paths = {}, -- add any custom paths here that you want to includes in the rtp
      ---@type string[] list any plugins you want to disable here
      disabled_plugins = {
        -- "gzip",
        -- "matchit",
        -- "matchparen",
        -- "netrwPlugin",
        -- "tarPlugin",
        -- "tohtml",
        -- "tutor",
        -- "zipPlugin",
      },
    },
  },
}

-- loading list from basd46
local packages = {
  "defaults",
  "syntax",
  "treesitter",
  "telescope",
  "cmp",
  "nvcheatsheet",
  "colors",
  "lsp",
  "git",
  "mason",
  "tbline",
  "term",
  "nvimtree",
  -- "statusline",
  -- "whichkey"
  -- "blankline"
  -- "devicons"
}

-- load base46 UI
for _, v in ipairs(packages) do
  dofile(vim.g.base46_cache .. v)
end


-- load basd46 UI all
-- for _, v in ipairs(vim.fn.readdir(vim.g.base46_cache)) do
--   dofile(vim.g.base46_cache .. v)
-- end

-- add binaries installed by mason.nvim to path
local is_windows = vim.fn.has "win32" ~= 0
local sep = is_windows and "\\" or "/"
local delim = is_windows and ";" or ":"

-- vim.env.PATH = table.concat({ vim.fn.stdpath "data", "mason", "bin" }, sep) .. delim .. vim.env.PATH
