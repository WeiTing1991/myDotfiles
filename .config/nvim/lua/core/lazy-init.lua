local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  intall = {
    colorscheme = { "rose-pine", "catppuccin" },
  },
  spec = {
    LAZY_PLUGIN_SPEC,
    { "tpope/vim-sleuth", event = "BufEnter" },
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
        --"zipPlugin",
      },
    },
  },
})
