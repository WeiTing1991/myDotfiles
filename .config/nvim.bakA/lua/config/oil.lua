-- https://github.com/stevearc/oil.nvim

require("oil").setup ({
  default_file_explorer = false,
  delete_to_trash = true,
  skip_confirm_for_simple_edits = true,
  view_options = {
    show_hidden = true,
    natural_order = true,
    is_always_hidden = function(name, _)
      return name == ".DS_Store" or name == ".." or name == ".git"
    end,
  },
  float = {
    padding = 2,
    max_width = 120,
    max_height = 0,
    border = "rounded",
    win_options = {
      winblend = 0,
    },
    preview_split = "below",
  },
  keymaps = {
    ["<C-c>"] = false,
    ["q"] = "actions.close",
  },
})
