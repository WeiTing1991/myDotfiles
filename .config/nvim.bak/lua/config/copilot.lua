require('copilot').setup({
  panel = {
    enabled = false,
    auto_refresh = false,
    keymap = {
      jump_prev = "[[",
      jump_next = "]]",
      accept = "<CR>",
      refresh = "gr",
      open = "<M-CR>"
    },
    layout = {
      position = "vertical", -- | top | left | right | horizontal | vertical
      ratio = 0.4
    },
  },
  suggestion = {
    enabled = true,
    auto_trigger = true,
    hide_during_completion = true,
    debounce = 10,
    keymap = {
      accept = "<C-l>",
      accept_word = "<C-f>",
      accept_line = false,
      next = "<C-]>",
      prev = "<C-[>",
      dismiss = "<Esc>",
      -- dismiss = "<C-c>",
    },
  },
  filetypes = {
    yaml = false,
    markdown = true,
    help = false,
    gitcommit = false,
    gitrebase = false,
    hgcommit = false,
    svn = false,
    cvs = false,
    ["."] = false,
  },
  -- copilot_node_command = 'node', -- Node.js version must be > 18.x
  server_opts_overrides = {},
})

