local builtin = require "telescope.builtin"
local utils = require "telescope.utils"

-- setup
require("telescope").setup {
  defaults = {
    vimgrep_arguments = {
    },
    layout_config = {
      horizontal = {
        prompt_position = "bottom",
        preview_width = 0.65,
      },
      vertical = {
        mirror = false,
      },
      width = 0.85,
      height = 0.90,
      preview_cutoff = 80,
    },
  },
  pickers = {
    find_files = {
      hidden = true,
      find_command = {
        "rg",
        "-L",
        "--files",
        "--hidden",
        "--glob",
        "!**/.git/*",
      },
    },
  },
}

pcall(require("telescope").load_extension, "fzf")

-- keymaps
vim.keymap.set("n", "<leader>f", builtin.find_files, { desc = "Find Files" })
vim.keymap.set("n", "<leader>sk", builtin.keymaps, { desc = "Search Keymaps" })
vim.keymap.set("n", "<leader><leader>", builtin.buffers, { desc = "Find file in buffer" })
vim.keymap.set("n", "<space>ff", function() builtin.find_files { cwd = utils.buffer_dir() } end,
  { desc = "file browser in buffer" })
