-- telescoper config

local builtin = require "telescope.builtin"
local utils = require "telescope.utils"
local actions = require "telescope.actions"

pcall(require("telescope").load_extension, "fzf")
-- pcall(require("telescope").load_extension, "frecency")
-- pcall(require("telescope").load_extension "ui-select")

-- keymaps
vim.keymap.set("n", "<leader>f", builtin.find_files, { desc = "Find files" })
vim.keymap.set("n", "<leader>fk", builtin.keymaps, { desc = "Search keymaps" })
vim.keymap.set("n", "<leader><leader>", builtin.buffers, { desc = "Find existing buffer" })

-- vim.keymap.set("n", "<space>ff", function()
--   builtin.find_files { cwd = utils.buffer_dir() }
-- end, { desc = "file browser in buffer" })
-- vim.keymap.set("n", "<leader>fl", function()
--   builtin.live_grep { grep_open_files = true }
-- end, { desc = "Find live grep" })
--
-- vim.keymap.set("n", "<leader>fs", function()
--   builtin.grep_string { search = vim.fn.input "Grep > " }
-- end, { desc = "Grep search" })
-- --
-- -- word search
-- vim.keymap.set("n", "<leader>fw", function()
--   local word = vim.fn.expand "<cword>"
--   builtin.grep_string { search = word }
-- end, { desc = "word search" })
-- vim.keymap.set("n", "<leader>fW", function()
--   local word = vim.fn.expand "<cWORD>"
--   builtin.grep_string { search = word }
-- end, { desc = "cWord search" })
