----- Pluglins keymapsj-----
local map = vim.keymap.set
-- local tele_builtin = require("telescope.builtin")
local snacks = require("snacks")
local neotree = require("neo-tree.command")
local fzf = require("fzf-lua")

--[[ fzf/search ]]
map("n", "<C-p>", fzf.files, { desc = "Find files" })
map("n", "<leader>ff", fzf.files, { desc = "Find files" })
map("n", "<leader>fl", fzf.live_grep, { desc = "Find live grep" })
map("n", "<leader>fg", fzf.grep_curbuf, { desc = "Grep in current buffer" })
map("n", "<leader>fb", fzf.buffers, { desc = "Find file in opened buffer" })
map("n", "<leader><leader>", fzf.buffers, { desc = "Find file in opened buffer" })
-- map("n", "<leader>fo", tele_builtin.oldfiles, { desc = "Open recent file" })


-- [[ Override default keymaps ]]
map("n", "<C-q>", function()
  snacks.bufdelete()
end, { desc = "Close current buffer and window" })

-- [[ File tree ]]
map("n", "<leader>d", function()
  require("oil").open()
end, { desc = "Toggle file explorer" })
map("n", "<C-e>", function()
  neotree.execute({ toggle = true, dir = vim.uv.cwd() })
end, { desc = "File tree" })
-- Not working on warp terminal
map("n", "<C-S-e>", function()
  require("sidekick.cli").toggle({ name = "claude", focus = true })
end, { desc = "Toggle AI Chat" })

-- [[ Spell suggestion ]]
map("n", "z=", function()
  require("fzf-lua").spell_suggest()
end, { desc = "Spell Suggestion" })

-- [[ Terminal ]]
map({ "n", "t" }, "<C-`>", function()
  snacks.terminal()
end, { desc = "Toggle terminal" })
-- fallback
map({ "n", "t" }, "<leader>/", function()
  snacks.terminal()
end, { desc = "Toggle terminal" })
map("n", "<leader>tt", function()
  require("nvchad.themes").open()
end, { desc = "Toggle colorscheme" })

-- [[ Spell check ]]
map("n", "<leader>tp", function()
  vim.g.spell_enabled = not vim.g.spell_enabled
  if vim.g.spell_enabled then
    -- Enabled: run spell check immediately
    local first_line = vim.fn.line("w0") - 1
    local last_line = vim.fn.line("w$")
    require("fastspell").sendSpellCheckRequest(first_line, last_line)
    vim.notify("Spell check enabled", vim.log.levels.INFO)
  else
    -- Disabled: clear all diagnostics
    require("fastspell").sendSpellCheckRequest(0, 0)
    vim.notify("Spell check disabled", vim.log.levels.INFO)
  end
end, { desc = "Spell Ignore" })
-- [[ toggle ]]
map("n", "<leader>tc", function()
  require("copilot.suggestion").toggle_auto_trigger()
  if not vim.b.copilot_suggestion_auto_trigger then
    print("Copilot is disabled")
  else
    print("Copilot is enabled")
  end
end, { desc = "Copilot" })

-- [[ Refactoring tool ]]
map("x", "<leader>re", ":Refactor extract ", { desc = "Extract Method" })
map("x", "<leader>rf", ":Refactor extract_to_file ", { desc = "Extract Method to File" })
map("x", "<leader>rv", ":Refactor extract_var ", { desc = "Extract Variable" })
map({ "n", "x" }, "<leader>ri", ":Refactor inline_var", { desc = "Inline Variable" })
map( "n", "<leader>rI", ":Refactor inline_func", { desc = "Inline Function" })
map("n", "<leader>rb", ":Refactor extract_block", { desc = "Extract Block" })
map("n", "<leader>rbf", ":Refactor extract_block_to_file", { desc = "Extract Block to File" })

-- [[ Tasks runner ]]
map("n", "<leader>tr", "<cmd>OverseerRun<cr>", { desc = "Run task" })
-- map("n", "<leader>ti", "<cmd>OverseerInfo<cr>", { desc = "Task info" })
-- map("n", "<leader>tk", "<cmd>OverseerToggle<cr>", { desc = "Task toggle" })

--[[ Diagnostics ]]
map("n", "<leader>xd", "<cmd>Trouble diagnostics toggle filter.buf=0<cr>", { desc = "Diagnostics " })
map("n", "<leader>xw", "<cmd>Trouble diagnostics toggle<cr>", { desc = "Diagnostics workspace" })
map("n", "<leader>xq", "<cmd>Trouble qflist toggle <cr>", { desc = "Quickfix List " })
map("n", "<leader>xl", "<cmd>Trouble locflist toggle <cr>", { desc = "Location List " })

vim.keymap.set("n", "]t", function()
  require("trouble").next({ skip_groups = true, jump = true })
end, { desc = "Next Trouble item" })

vim.keymap.set("n", "[t", function()
  require("trouble").prev({ skip_groups = true, jump = true })
end, { desc = "Previous Trouble item" })

-- TOD:
-- [[ Git ]]
-- map("n", "<C>G", "<cmd>Neotree git_status toggle<cr>", { desc = "Tree Git" })
map("n", "<leader>gh", ":Gitsign preview_hunk<CR>", { desc = "Preview hunk" })
map("n", "<leader>gb", ":Gitsign blame<CR>", { desc = "Git blame" })
map("n", "<leader>gg", ":LazyGit<CR>", { desc = "LazyGit" })
map("n", "<leader>gr", function()
  snacks.picker.gh_pr()
end, { desc = "Show PR" })

-- map("n", "<leader>gd", ":DiffviewOpen<cr>", { desc = "Git diff view" })
-- map("n", "<leader>gf", ":DiffviewFileHistory<cr>", { desc = "Git diff view" })
-- map("n", "<leader>gq", ":DiffClose<cr>", { desc = "Close GitDiff" })

-- map("n", "<leader>gb", function()
--   snacks.gitbrowse()
-- end, { desc = "open current github" })
--
-- local function commit_files()
--   local message = vim.fn.input("Commit message: ")
--   vim.cmd("Git add .")
--   vim.cmd('Git commit -m "' .. message .. '"')
-- end
-- map("n", "<leader>gc", commit_files, { desc = "Git commit current file" })
-- map({ "n", "v" }, "<leader>gH", function()
--   require("gitsigns").stage_hunk({ vim.fn.line("."), vim.fn.line("v") })
-- end, { desc = "Stage hunk" })

-- map("n", "<leader>ghs", ":Gitsign stage_buffer<CR>", { desc = "stage hunk" })
-- map("n", "<leader>ghh", ":Gitsign preview_hunk<CR>", { desc = "Preview hunk" })
-- map({ "n", "v" }, "<leader>ghr", function()
--   require("gitsigns").reset_hunk({ vim.fn.line("."), vim.fn.line("v") })
-- end, { desc = "git reset hunk" })
-- map("n", "<leader>gss", ":Gitsign stage_hunk<CR>", { desc = "stage hunk" })
-- map("n", "<leader>ghr", ":Gitsign reset_hunk<CR>", { desc = "reset hunk" })

