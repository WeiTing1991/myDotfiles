----- Pluglins keymaps -----
local map = vim.keymap.set
local ui_select = require("core.select")
local tele_builtin = require("telescope.builtin")
local tele = require("telescope")
local snacks = require("snacks")
local neotree = require("neo-tree.command")

--[[ telescope/search ]]
map("n", "<C-f>", tele_builtin.find_files, { desc = "Find files" })
map("n", "<leader>ff", tele_builtin.find_files, { desc = "Find files" })
map("n", "<leader>fl", tele_builtin.live_grep, { desc = "Find live grep" })
-- map("n", "<leader>fo", tele_builtin.oldfiles, { desc = "Open recent file" })
-- map("n", "<leader>fb", tele_builtin.buffers, { desc = "Find file in opened buffer" })
-- map("n", "<leader>ol", "<cmd>Telescope aerial<cr>", { desc = "Find the project outline" })

-- [[ File tree ]]
map("n", "<leader>d", function() require("oil").open() end, { desc = "Toggle file explorer" })
map("n", "<C-e>",  function()
  neotree.execute({toggle = true, dir = vim.uv.cwd()})
end, { desc = "File tree" })

-- [[ Override default keymaps ]]
map("n", "<C-q>", function() snacks.bufdelete() end, { desc = "Close current buffer and window" })

-- [[ Terminal ]]
map({ "n", "t" }, "<C-/>", function() snacks.terminal() end, { desc = "Toggle terminal" })

map("n", "<leader>tt",  function() snacks.picker.colorschemes() end, { desc = "Toggle colorscheme" })

-- [[ Git ]]
map("n", "<C-S-g>",  "<cmd>LazyGit<cr>", { desc = "LazyGit" })
