local nvimtree = require("nvim-tree")
local icon = require("icon")

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

local function my_on_attach(bufnr)
  local api = require("nvim-tree.api")
  local function opts(desc)
    return { desc = "nvim-tree: " .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
  end
  -- default mappings
  api.config.mappings.default_on_attach(bufnr)
  vim.keymap.set("n", "n", api.fs.create, opts "Create File Or Directory")
  vim.keymap.set("n", "<C-S-e>", api.tree.close, opts("Toggle File Explorer"))
end

nvimtree.setup({
  disable_netrw = false,
  sync_root_with_cwd = true,
  --   hijack_netrw = true,
  update_focused_file = {
    enable = true,
    update_root = false,
  },
  sort = {
    sorter = "case_sensitive",
  },
  view = {
    width = 40,
    relativenumber = false,
    side = "right",

    -- float settings
    float = {
      enable = false,
      quit_on_focus_loss = true,
      open_win_config = function()
        local screen_w = vim.opt.columns:get()
        local screen_h = vim.opt.lines:get() - vim.opt.cmdheight:get()
        local window_w = screen_w * 0.35
        local window_h = screen_h * 0.95
        local window_w_int = math.floor(window_w)
        local window_h_int = math.floor(window_h)
        local center_x = vim.opt.winwidth:get() + (vim.opt.lines:get() + window_w)
        local center_y = ((vim.opt.lines:get() - window_h) / 2) - vim.opt.cmdheight:get()
        return {
          border = "rounded",
          relative = "editor",
          row = center_y,
          col = center_x,
          width = window_w_int,
          height = window_h_int,
        }
      end,
    },
  },

  on_attach = my_on_attach,
  renderer = {
    indent_markers = { enable = true },
    highlight_git = false,
    icons = {
      glyphs = icon.tree.glyphs,
      -- git = { unmerged = "" },
    },
  },
  actions = {
    open_file = {
      window_picker = {
        enable = false,
      },
    },
  },
  filters = {
    dotfiles = false,
    custom = {
      ".DS_Store",
    },
    exclude = {
      ".git",
      ".cache",
      "node_modules",
      ".venv",
      ".env",
    },
  },
  git = {
    ignore = true,
  },
})
