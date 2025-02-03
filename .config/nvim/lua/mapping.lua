local map = vim.keymap.set

-- keymaps
-- fzf
-- check https://github.com/nvim-telescope/telescope.nvim?tab=readme-ov-file#pickers
local builtin = require "telescope.builtin"

map("n", "<C-f>", builtin.find_files, { desc = "Find files" })
map("n", "<leader>f", builtin.find_files, { desc = "Find files" })
map("n", "<leader>b", function()
  builtin.find_files {cwd = utils.buffer_dir()}
end, { desc = "Finde file in current child dir" })
map("n", "<leader>fo", builtin.oldfiles, { desc = "Open recent file" })
--
map("n", "<leader>fb", builtin.buffers, { desc = "Find existing buffer" })
map("n", "<leader>fk", builtin.keymaps, { desc = "Search keymaps" })
map("n", "<leader>fg", function()
    builtin.grep_string { search = vim.fn.input "Grep > " }
  end, { desc = "Grep search" })

map("n", "<leader>fl", builtin.live_grep, { desc = "Find live grep" })
--
-- -- vim.keymap.set("n", "<C-g>", builtin.git_files, { desc = "Find live grep" })
--

-- -- word search
-- vim.keymap.set("n", "<leader>fw", function()
--   local word = vim.fn.expand "<cword>"
--   builtin.grep_string { search = word }
-- end, { desc = "word search" })

-- vim.keymap.set("n", "<leader>fW", function()
--   local word = vim.fn.expand "<cWORD>"
--   builtin.grep_string { search = word }
-- end, { desc = "cWord search" })
