vim.g.mapleader = " "
vim.g.maplocalleader = ","

require "core.options"
require "core.keymaps"
require "core.autocmds"

-- Plugin manager
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
require('lazy').setup({
  spec = {
  { import = "plugin.init" },
  { import = "plugin.editor" },
  { import = "plugin.lsp" },
  { import = "plugin.lsp_enhance" },
  { import = "plugin.snack" },
  { import = "plugin.git" },
  { import = "plugin.ai" },
  { import = "plugin.tools" },
  { import = "plugin.ui" },
  -- { import = "plugin.note_taking" },
  -- { import = "plugin.sql" },
  -- { import = "plugin.dap" },
  },
  defaults = { lazy = false, version = false },
  ui = {
    border = "rounded",
    size = { width = 0.8, height = 0.8 },
  },
  rocks = {
    enabled = false,
  },
  change_detection = {
    enabled = true,
    notify = false,
  },
  checker = {
    enabled = true,
    notify = false,
  },
  performance = {
    -- reset_packpath = true,
    rtp = {
      disabled_plugins = {
        "gzip",
        -- "matchit",
        -- "matchparen",
        -- "netrwPlugin",
        "tarPlugin",
        "tohtml",
        "tutor",
        "zipPlugin",
        -- "rplugin",
      },
    },
  },
})

require "userkeymaps"

vim.api.nvim_create_autocmd("VimEnter", {
  callback = function()
    print("Startup time: " .. vim.fn.reltimestr(vim.fn.reltime(vim.g.start_time)) .. "s")
  end,
})
vim.g.start_time = vim.fn.reltime()
