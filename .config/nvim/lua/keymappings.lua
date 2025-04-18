----- Pluglins keymaps -----
local map = vim.keymap.set
local fzf = require "fzf-lua"


-- file tree
map("n", "<leader>d", function()
  require("oil").open()
end, { desc = "Toggle file explorer" })

-- fzf
map("n", "<C-f>", fzf.files, { desc = "Find files" })
map("n", "<leader>ff", fzf.files, { desc = "Find files" })
map("n", "<leader><leader>", function()
  require("fzf-lua").files { cwd = vim.fn.expand "%:p:h" }
end, { desc = "Finde file in current child dir" })
map("n", "<leader>fo", fzf.oldfiles, { desc = "Open recent file" })
map("n", "<leader>fb", fzf.buffers, { desc = "Finde file in opened buffer" })

map("n", "<leader>fg", function()
  fzf.grep { search = vim.fn.input "Grep > " }
end, { desc = "Grep search" })

map("n", "<leader>fk", fzf.keymaps, { desc = "Search keymaps" })
map("n", "<leader>fl", fzf.live_grep, { desc = "Find live grep" })

map("n", "<leader>bb", fzf.tabs, { desc = "Find tabs" })
map("n", "<leader>bt", fzf.tmux_buffers, { desc = "Find tmux buffer" })
