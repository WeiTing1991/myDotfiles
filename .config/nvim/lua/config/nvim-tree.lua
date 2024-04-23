local nvimtree = require('nvim-tree')

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
-- change color for arrows in tree to light blue check this
-- https://raw.githubusercontent.com/kyazdani42/nvim-tree.lua/master/doc/nvim-tree-lua.txt

nvimtree.setup({
  disable_netrw = true,
  hijack_netrw = true,
  update_focused_file = {
    enable = true,
    update_cwd = true,
  },
  view = {
    width = 30,
    relativenumber = true,
    -- float settings
    float = {
      enable = true,
      quit_on_focus_loss = true,
      open_win_config = function()
        local screen_w = vim.opt.columns:get()
        local screen_h = vim.opt.lines:get() - vim.opt.cmdheight:get()
        local window_w = screen_w * 0.4
        local window_h = screen_h * 0.9
        local window_w_int = math.floor(window_w)
        local window_h_int = math.floor(window_h)
        local center_x = 0
        local center_y = ((vim.opt.lines:get() - window_h) / 2) - vim.opt.cmdheight:get()
        return {
          border = 'rounded',
          relative = 'editor',
          row = center_y,
          col = center_x,
          width = window_w_int,
          height = window_h_int,
        }
      end,
    },
  },
  renderer = {
    indent_markers = {
      enable = true,
    },
    highlight_git = true,
    icons = {
      glyphs = {
        folder = {
          arrow_closed = '- ', -- arrow when folder is closed
          arrow_open = '|', -- arrow when folder is open
        },
      },
    },
  },
  -- disable window_picker for
  -- explorer to work well with
  -- window splits
  actions = {
    open_file = {
      window_picker = {
        enable = false,
      },
    },
  },
  filters = {
    custom = { '.DS_Store' },
  },
  git = {
    ignore = false,
  },
})

