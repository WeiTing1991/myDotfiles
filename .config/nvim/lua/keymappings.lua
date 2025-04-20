----- Pluglins keymaps -----
local map = vim.keymap.set
local fzf = require "fzf-lua"

-- file tree
map("n", "<leader>d", function()
  require("oil").open()
end, { desc = "Toggle file explorer" })

map("n", "<C-e>", "<cmd>NvimTreeToggle<cr>", { desc = "File tree" })

-- fzf
map("n", "<C-f>", fzf.files, { desc = "Find files" })
map("n", "<leader>ff", fzf.files, { desc = "Find files" })
map("n", "<leader>fo", fzf.oldfiles, { desc = "Open recent file" })
map("n", "<leader><leader>", fzf.buffers, { desc = "Finde file in opened buffer" })
map("n", "<leader>fb", function()
  require("fzf-lua").files { cwd = vim.fn.expand "%:p:h" }
end, { desc = "Finde file in current child dir" })
map("n", "<leader>fg", function()
  fzf.grep { search = vim.fn.input "Grep > " }
end, { desc = "Grep search" })
map("n", "<leader>fk", fzf.keymaps, { desc = "Search keymaps" })
map("n", "<leader>fl", fzf.live_grep, { desc = "Find live grep" })
map("n", "<leader>ft", fzf.tabs, { desc = "Find tabs" })
map("n", "<leader>ftb", fzf.tmux_buffers, { desc = "Find tmux buffer" })

map("n", "z=", fzf.spell_suggest, { desc = "Spelling suggestions" })

-- Markdown
-- map("n", "<leader>mr", "<cmd>Markview<cr>", { desc = "markdown render toggle" })
map("n", "<leader>mp", "<cmd>MarkdownPreviewToggle<cr>", { desc = "markdown preview with node" })


-- git
map("n", "<leader>gd", ":DiffviewOpen<cr>", { desc = "Git diff view" })
map("n", "<leader>gf", ":DiffviewFileHistory<cr>", { desc = "Git diff view" })

map("n", "<leader>gl", function()
  require("core.float_term").float_term(
    "lazygit",
    { size = { width = 0.85, height = 0.8 }, cwd = vim.b.gitsigns_status_dict.root }
  )
end, { desc = "Lazygit" })

-- toggle
map("n", "<leader>tc", function()
  require("copilot.suggestion").toggle_auto_trigger()
  if not vim.b.copilot_suggestion_auto_trigger then
    print "Copilot is disabled"
  else
    print "Copilot is enabled"
  end
end, { desc = "Copilot" })

