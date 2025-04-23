local alpha = require "alpha"
local dashboard = require "alpha.themes.dashboard"

-- Set header
dashboard.section.header.val = {
  "                                              ",
  "                                              ",
  "                                              ",
  "                                              ",
  "                                              ",
  "                                              ",
  "                                              ",
  "                                              ",
  "██     █████████████    ████    ███████    ███",
  "██     ██   ██   ████   ████    ████████  ████",
  "██  █  ██   ██   ██ ██  ████    ██████ ████ ██",
  "██ ███ ██   ██   ██  ██ ██ ██  ██ ████  ██  ██",
  " ███ ███    ██   ██   ████  ████  ████      ██",
  "                                              ",
  "                                              ",
  "             Powered By  eovim              ",
  "                                              ",
  "                                              ",
  "                                              ",
}

-- Set menu
dashboard.section.buttons.val = {
  dashboard.button("ff", "  Find File", ":FzfLua files<CR>"),
  dashboard.button("fo", "  Recent Files", ":FzfLua oldfiles<CR>"),
  dashboard.button("fl", "󰈭  Find Word", ":FzfLua live_grep<CR>"),
  dashboard.button("d", "󱥚  Dired", ":lua require('oil').open()<CR>"),
  dashboard.button("e", "󱥚  Tree", ":NvimTreeToggle<CR>"),
  -- dashboard.button("th", "󱥚  Themes", ":lua require('nvchad.themes').open()<CR>"),
}

-- local function footer()
--   local elapsed = os.clock() - vim.g.start_time
--   local startup_time = string.format("%.3f", elapsed)
--   local stats = require("lazy").stats()
--   local ms = math.floor(stats.startuptime) .. " ms"
--   local line1 = "─────────────────────────────────────────"
--   local line2 = "  Loaded " .. stats.loaded .. "/" .. stats.count .. " plugins in " .. ms
--   local line3 = "  Startup time: " .. startup_time .. "s"
--   local line4 = "─────────────────────────────────────────"
--   return line1 .. "\n" .. line2 .. "\n" .. line3 .. "\n" .. line4
-- end
--
-- dashboard.section.footer.val = footer()

-- Send config to alpha
alpha.setup(dashboard.opts)

-- Disable folding on alpha buffer
vim.cmd [[
    autocmd FileType alpha setlocal nofoldenable
]]
