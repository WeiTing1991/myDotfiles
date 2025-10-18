-- local obPath = vim.fn.expand("~/Library/Mobile Documents/iCloud~md~obsidian/Documents/weitingchen/")

local is_windows = vim.loop.os_uname().sysname == "Windows_NT"
local is_mac = vim.loop.os_uname().sysname == "Darwin"

local dashboard = {
  width = 60,
  row = nil, -- dashboard position. nil for center
  col = nil, -- dashboard position. nil for center
  pane_gap = 4, -- empty columns between vertical panes
  autokeys = "1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", -- autokey sequence
  -- These settings are used by some built-in sections
  preset = {
    ---@type fun(cmd:string, opts:table)|nil
    pick = nil,
    ---@type snacks.dashboard.Item[]
    keys = {
      { icon = " ", key = "f", desc = "Find File", action = ":FzfLua files" },
      -- { icon = " ", key = "n", desc = "New File", action = ":ene | startinsert" },
      { icon = " ", key = "o", desc = "Recent Files", action = ":FzfLua oldfiles" },
      { icon = " ", key = "g", desc = "LazyGit", action = ":LazyGit" },
      {
        icon = " ",
        key = "c",
        desc = "Config",
        action = ":lua Snacks.dashboard.pick('files', {cwd = vim.fn.stdpath('config')})",
      },
      -- {
      --   icon = " ",
      --   key = "b",
      --   desc = "Notes",
      --   action = string.format(":lua Snacks.dashboard.pick('files', {cwd = '%s'})", obPath),
      -- },
      -- { icon = "󱥚 ", key = "d", desc = "Dired", action = ":lua require('oil').open()<CR>" },
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

local default_shell = ""
if is_mac then
  default_shell = "/bin/zsh"
elseif is_windows then
  default_shell = "pwsh.exe" or "cmd.exe"
end

return {
  "WeiTing1991/snacks.nvim",
  priority = 1000,
  lazy = false,
  opts = {
    dashboard = dashboard,
    terminal = {
      enabled = true,
      shell = default_shell,
    },
    indent = {
      enabled = true,
      animate = {
        enabled = false,
      },
      scope = {
        enabled = true,
        priority = 200,
        char = "▏",
      },
    },
    statuscolumn = {
      enabled = true,
      left = { "sign", "git" },
      right = { "mark", "fold" },
      folds = {
        open = false,
        git_hl = false,
      },
      git = {
        patterns = { "GitSign" },
      },
      refresh = 50,
    },
    animate = {
      enabled = true,
    },
    git = { enabled = false },
    explorer = { enabled = false },
    bigfile = { enabled = false },
    input = { enabled = false },
    notifier = { enabled = false },
    quickfile = { enabled = false },
    scope = { enabled = false },
    scroll = { enabled = false },
    words = { enabled = false },
    picker = {
      enabled = false,
      sources = {
        explorer = {
          layout = {
            layout = {
              position = "right",
            },
          },
        },
      },
    },
  },
  config = function(_, opts)
    require("snacks").setup(opts)
  end,
}
