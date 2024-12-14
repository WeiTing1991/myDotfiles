  -- disable
  -- obsidian
  {
    "epwalsh/obsidian.nvim",
    enabled = false,
    version = "*",  -- recommended, use latest release instead of latest commit
    lazy = true,
    ft = "markdown",
    dependencies = {
      "nvim-lua/plenary.nvim",
    },
  config = function()

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
        ui = { enable = true },
        workspaces = {
          {
            name = "notes",
            path = obsidian_path,
          },
          {
            name = "no-vault",
            path = function()
              -- alternatively use the CWD:
              -- return assert(vim.fn.getcwd())
              return assert(vim.fs.dirname(vim.api.nvim_buf_get_name(0)))
            end,
            overrides = {
              notes_subdir = vim.NIL,  -- have to use 'vim.NIL' instead of 'nil'
              new_notes_location = "current_dir",
              templates = {
                folder = vim.NIL,
              },
              disable_frontmatter = true,
            },
          },
        },
        completion = {
          nvim_cmp = false,
          min_chars = 2,
        },
      }
  end
}
