local builtin = require "telescope.builtin"

-- setup
require('telescope').setup {}

pcall(require("telescope").load_extension, "fzf")

-- keymaps
vim.keymap.set("n", "<leader>f", builtin.find_files, { desc = "Find Files" })
vim.keymap.set("n", "<leader>sk", builtin.keymaps, { desc = "Search Keymaps" })
vim.keymap.set("n", "<leader><leader>", builtin.buffers, { desc = "Find file in buffer" })
