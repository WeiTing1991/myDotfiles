local builtin = require "telescope.builtin"

-- setup
require("telescope").setup {
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
}

pcall(require("telescope").load_extension, "fzf")

-- keymaps
vim.keymap.set("n", "<leader>f", builtin.find_files, { desc = "Find Files" })
vim.keymap.set("n", "<leader>sk", builtin.keymaps, { desc = "Search Keymaps" })
vim.keymap.set("n", "<leader><leader>", builtin.buffers, { desc = "Find file in buffer" })
