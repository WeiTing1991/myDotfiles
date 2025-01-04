-- Note: https://github.com/stevearc/oil.nvim?tab=readme-ov-file#quick-start

require("oil").setup {
  default_file_explorer = false,
  delete_to_trash = true,
  skip_confirm_for_simple_edits = true,
  win_options = {
    wrap = false,
    signcolumn = "no",
    cursorcolumn = false,
    foldcolumn = "0",
    spell = false,
    list = false,
    conceallevel = 3,
    concealcursor = "nvic",
  },
  view_options = {
    show_hidden = true,
    natural_order = true,
    is_always_hidden = function(name, _)
      return name == ".DS_Store" or name == ".." or name == ".git"
    end,
  },
  float = {
    padding = 2,
    max_width = 0,
    max_height = 0,
    border = "rounded",
    win_options = {
      winblend = 0,
    },
    preview_split = "below",
  },
  preview_win = {
    update_on_cursor_moved = true,
    -- How to open the preview window "load"|"scratch"|"fast_scratch"
    preview_method = "fast_scratch",
    disable_preview = function(filename)
      return false
    end,
    win_options = {},
  },
  keymaps = {
    ["g?"] = { "actions.show_help", mode = "n" },
    ["<C-c>"] = false,
    ["<C-p>"] = "actions.preview",
    ["q"] = "actions.close",
  },
}
