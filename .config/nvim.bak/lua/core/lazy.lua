-- lazy settings.
return {
  defaults = { lazy = true, version = false },
  install = {
    missing = false,
  },
  ui = {
    border = "rounded",
    size = { width = 0.8, height = 0.8 },
    -- icons = {
    --   ft = "",
    --   lazy = "󰂠 ",
    --   loaded = "",
    --   not_loaded = "",
    -- },
  },
  rocks = {
    enabled = false,
  },
  change_detection = {
    enabled = true,
    notify = false, -- get a notification when changes are found
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
        "netrwPlugin",
        "tarPlugin",
        "tohtml",
        "tutor",
        "zipPlugin",
        "rplugin",
        "tohtml",
        "tutor",
      },
    },
  },
}
