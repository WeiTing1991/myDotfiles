require("obsidian").setup {
  workspaces = {
    {
      name = "notes",
      path = "/Users/weitingchen/Library/Mobile Documents/iCloud~md~obsidian/Documents/weitingchen/",
    },
  },
  completion = {
    nvim_cmp = true,
    min_chars = 2,
  },
  -- new_note_location = 'current_dir',
}
