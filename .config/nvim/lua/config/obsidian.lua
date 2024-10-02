
local obsidian_path

if vim.loop.os_uname().sysname == "Darwin" then
  -- macOS
  obsidian_path = "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/weitingchen/"
elseif vim.fn.has "Win32" then
  -- Windows
  obsidian_path = "C:/Users/weitingchen/iCloudDrive/iCloud~md~obsidian/weitingchen" -- Adjust this path as needed
else
  obsidian_path = "~/Documents/Obsidian/weitingchen/" -- Default or Linux path
end

require("obsidian").setup {
  ui = { enable = false },
  workspaces = {
    {
      name = "notes",
      path = obsidian_path,
    },
  },
  completion = {
    nvim_cmp = true,
    min_chars = 2,
  },
  -- new_note_location = 'current_dir',
}
