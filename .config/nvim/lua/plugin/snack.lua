-- local obPath = vim.fn.expand("~/Library/Mobile Documents/iCloud~md~obsidian/Documents/weitingchen/")

local dashboard = {
  width = 60,
  row = nil, -- dashboard position. nil for center
  col = nil, -- dashboard position. nil for center
  pane_gap = 4, -- empty columns between vertical panes
  autokeys = "1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", -- autokey sequence
  -- These settings are used by some built-in sections
  preset = {
    -- Defaults to a picker that supports `fzf-lua`, `telescope.nvim` and `mini.pick`
    ---@type fun(cmd:string, opts:table)|nil
    pick = nil,
    -- Used by the `keys` section to show keymaps.
    -- Set your custom keymaps here.
    -- When using a function, the `items` argument are the default keymaps.
    ---@type snacks.dashboard.Item[]
    keys = {
      { icon = " ", key = "f", desc = "Find File", action = ":Telescope find_files" },
      -- { icon = " ", key = "n", desc = "New File", action = ":ene | startinsert" },
      -- { icon = " ", key = "g", desc = "Find Text", action = ":" },
      { icon = " ", key = "o", desc = "Recent Files", action = ":Telescope oldfiles" },
      {
        icon = " ",
        key = "c",
        desc = "Config",
        action = ":lua Snacks.dashboard.pick('files', {cwd = vim.fn.stdpath('config')})",
      },
      {
        icon = " ",
        key = "b",
        desc = "Notes",
        action = string.format(":lua Snacks.dashboard.pick('files', {cwd = '%s'})", obPath),
      },
      { icon = "󱥚 ", key = "d", desc = "Dired", action = ":lua require('oil').open()<CR>" },
      -- { icon = " ", key = "s", desc = "Dired", action = ":lua require('oil').open()<CR>" },
      -- { icon = "󰒲 ", key = "L", desc = "Lazy", action = ":Lazy", enabled = package.loaded.lazy ~= nil },
      { icon = " ", key = "q", desc = "Quit", action = ":qa" },
    },
    header = [[
██     █████████████    ████    ███████    ███
██     ██   ██   ████   ████    ████████  ████
██  █  ██   ██   ██ ██  ████    ██████ ████ ██
██ ███ ██   ██   ██  ██ ██ ██  ██ ████  ██  ██
 ███ ███    ██   ██   ████  ████  ████      ██


 Powered By  eovim ]],
  },
  -- item field formatters
  formats = {
    icon = function(item)
      if item.file and item.icon == "file" or item.icon == "directory" then
        local M = require("icon")
        return M.icon(item.file, item.icon)
      end
      return { item.icon, width = 2, hl = "icon" }
    end,
    footer = { "%s", align = "center" },
    header = { "%s", align = "center" },
    file = function(item, ctx)
      local fname = vim.fn.fnamemodify(item.file, ":~")
      fname = ctx.width and #fname > ctx.width and vim.fn.pathshorten(fname) or fname
      if #fname > ctx.width then
        local dir = vim.fn.fnamemodify(fname, ":h")
        local file = vim.fn.fnamemodify(fname, ":t")
        if dir and file then
          file = file:sub(-(ctx.width - #dir - 2))
          fname = dir .. "/…" .. file
        end
      end
      local dir, file = fname:match("^(.*)/(.+)$")
      return dir and { { dir .. "/", hl = "dir" }, { file, hl = "file" } } or { { fname, hl = "file" } }
    end,
  },
  sections = {
    { section = "header" },
    { section = "keys", gap = 1, padding = 1 },
    { section = "startup" },
  },
}

return {
  "WeiTing1991/snacks.nvim",
  priority = 1000,
  lazy = false,
  opts = {
    bigfile = { enabled = false },
    dashboard = dashboard,
    explorer = { enabled = false },
    indent = { enabled = false },
    input = { enabled = false },
    picker = { enabled = false },
    notifier = { enabled = false },
    quickfile = { enabled = false },
    scope = { enabled = false },
    scroll = { enabled = false },
    statuscolumn = { enabled = false },
    words = { enabled = false },
  },
}
