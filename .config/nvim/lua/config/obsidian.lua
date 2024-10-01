require("obsidian").setup {
  ui = { enable = false },
  workspaces = {
    {
      name = "notes",
      path = "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/weitingchen/",
    },
  },
  completion = {
    nvim_cmp = true,
    min_chars = 2,
  },
  -- new_note_location = 'current_dir',
}
