local utils = require("core.utils")

local function spell_check()
  if vim.wo.spell then
    return vim.opt.spelllang:get()[1]
  else
    return ""
  end
end

local function copilot_status()
  if not vim.b.copilot_suggestion_auto_trigger then
    return " "
  else
    return " "
  end
end

local function indent_style()
  local style = vim.opt.expandtab:get() and "space" or "tab"
  local tab_width = vim.opt.shiftwidth:get()
  return string.format("%s(%d)", style, tab_width)
end

local default_shell = ""
if utils.is_mac then
  default_shell = "/bin/zsh"
elseif utils.is_windows then
  default_shell = "pwsh.exe"
end

return {
  -- Colorscheme
  {
    "WeiTing1991/gruvbox.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      vim.cmd.colorscheme("gruvbox")
    end,
  },

  -- Snacks (dashboard, terminal, indent, statuscolumn)
  {
    "WeiTing1991/snacks.nvim",
    lazy = false,
    priority = 1000,
    opts = {
      dashboard = {
        width = 60,
        preset = {
          keys = {
            { icon = " ", key = "f", desc = "Find File", action = ":FzfLua files" },
            { icon = " ", key = "l", desc = "Live Grep", action = ":FzfLua live_grep" },
            { icon = " ", key = "G", desc = "LazyGit", action = ":LazyGit" },
            {
              icon = " ",
              key = "c",
              desc = "Config",
              action = ":lua Snacks.dashboard.pick('files', {cwd = vim.fn.stdpath('config')})",
            },
            { icon = " ", key = "d", desc = "Dired", action = ":lua require('oil').open()" },
            { icon = " ", key = "q", desc = "Quit", action = ":qa" },
          },
          header = [[

          Powered By  eovim ]],
        },
        sections = {
          { section = "header" },
          { section = "keys", gap = 1, padding = 1 },
          { section = "startup" },
        },
      },
      terminal = {
        enabled = true,
        shell = default_shell,
      },
      indent = {
        enabled = true,
        animate = { enabled = false },
        scope = {
          enabled = true,
          priority = 200,
          char = "|",
        },
      },
      statuscolumn = {
        enabled = true,
        left = { "sign", "git" },
        right = { "mark", "fold" },
        folds = { open = false, git_hl = false },
        git = { patterns = { "GitSign" } },
        refresh = 50,
      },
      animate = { enabled = true },
      gh = {},
      git = { enabled = false },
      explorer = { enabled = false },
      bigfile = { enabled = false },
      input = { enabled = false },
      notifier = { enabled = false },
      quickfile = { enabled = false },
      scope = { enabled = false },
      scroll = { enabled = false },
      words = { enabled = false },
      picker = { enabled = false },
    },
    keys = {
      { "<C-`>", function() require("snacks").terminal() end, desc = "Toggle terminal", mode = { "n", "t" } },
    },
  },

  -- Statusline
  {
    "nvim-lualine/lualine.nvim",
    event = "VeryLazy",
    config = function()
      require("lualine").setup({
        options = {
          icons_enabled = true,
          theme = "auto",
          component_separators = { left = "", right = "" },
          section_separators = { left = "", right = "" },
          always_divide_middle = true,
          always_show_tabline = false,
          globalstatus = true,
          refresh = {
            statusline = 1000,
            tabline = 1000,
            winbar = 1000,
          },
        },
        sections = {
          lualine_a = { "mode" },
          lualine_b = { "branch", "diff", "diagnostics" },
          lualine_c = { { "filename", path = 2 } },
          lualine_x = { "encoding" },
          lualine_y = { indent_style, spell_check, copilot_status },
          lualine_z = { "location" },
        },
        inactive_sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = { "filename" },
          lualine_x = { "location" },
          lualine_y = {},
          lualine_z = {},
        },
      })
    end,
  },

  -- Breadcrumbs
  {
    "Bekaboo/dropbar.nvim",
    event = "BufReadPost",
    keys = {
      { "<Leader>;", function() require("dropbar.api").pick() end, desc = "Pick symbols in winbar" },
      { "[;", function() require("dropbar.api").goto_context_start() end, desc = "Go to start of context" },
      { "];", function() require("dropbar.api").select_next_context() end, desc = "Select next context" },
    },
  },

  -- Virtual column
  {
    "lukas-reineke/virt-column.nvim",
    event = "BufReadPost",
    opts = {
      char = { "|" },
      virtcolumn = "120",
      highlight = { "NonText" },
      exclude = { filetypes = { "oil", "markdown" } },
    },
  },
}
