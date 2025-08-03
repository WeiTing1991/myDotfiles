----- Pluglins keymaps -----
local map = vim.keymap.set
local tele_builtin = require("telescope.builtin")
local snacks = require("snacks")
local mini_ui_select = require("core.ui_select")

--[[ file tree ]]
map("n", "<leader>d", function()
  require("oil").open()
end, { desc = "Toggle file explorer" })
map("n", "<leader>e", "<cmd>NvimTreeToggle<cr>", { desc = "File tree" })
map("n", "<C-e>", "<cmd>NvimTreeToggle<cr>", { desc = "File tree" })

-- [[ Override default keymaps ]]
map("n", "<C-q>", function()
  snacks.bufdelete()
end, { desc = "Close current buffer and window" })

--[[ telescope/search ]]
map("n", "<C-f>", tele_builtin.find_files, { desc = "Find files" })
map("n", "<leader>ff", tele_builtin.find_files, { desc = "Find files" })
map("n", "<leader>fl", tele_builtin.live_grep, { desc = "Find live grep" })
map("n", "<leader>fo", tele_builtin.oldfiles, { desc = "Open recent file" })
map("n", "<leader>fb", tele_builtin.buffers, { desc = "Find file in opened buffer" })
map("n", "<leader>ol", "<cmd>Telescope aerial<cr>", { desc = "Find the project outline" })

map("n", "<leader>tt", function()
  mini_ui_select.ui_select(tele_builtin.colorscheme)
end, { desc = "toggle colorscheme" })

map("n", "z=", function()
  mini_ui_select.ui_select(tele_builtin.spell_suggest)
end, { desc = "spell suggestion" })

map({ "n", "t" }, "<C-/>", function()
  snacks.terminal()
end, { desc = "Toggle term" })


--[[ Markdown ]]
map("n", "<leader>mp", "<cmd>MarkdownPreviewToggle<cr>", { desc = "markdown preview with node" })

--[[ toggle ]]
map("n", "<leader>tc", function()
  require("copilot.suggestion").toggle_auto_trigger()
  if not vim.b.copilot_suggestion_auto_trigger then
    print("Copilot is disabled")
  else
    print("Copilot is enabled")
  end
end, { desc = "Copilot" })

-- map("n", "<leader>tu", vim.cmd.UndotreeToggle, { desc = "Undotree" })
-- map("n", "<leader>ta", function()
--   require("neogen").generate()
-- end, { desc = "Annotation" })

--[[ diagnostics ]]
map("n", "<leader>xd", "<cmd>Trouble diagnostics toggle filter.buf=0<cr>", { desc = "Diagnostics " })
map("n", "<leader>xw", "<cmd>Trouble diagnostics toggle<cr>", { desc = "Diagnostics workspace" })
map("n", "<leader>xq", "<cmd>Trouble qflist toggle <cr>", { desc = "Quickfix List " })
map("n", "<leader>xl", "<cmd>Trouble locflist toggle <cr>", { desc = "Location List " })

--[[ git ]]
-- https://www.naseraleisa.com/posts/diff
-- map("n", "<leader>gd", ":DiffviewOpen<cr>", { desc = "Git diff view" })
-- map("n", "<leader>gf", ":DiffviewFileHistory<cr>", { desc = "Git diff view" })
-- map("n", "<leader>gq", ":DiffClose<cr>", { desc = "Close GitDiff" })

map("n", "<leader>gb", ":Gitsign toggle_current_line_blame<CR>", { desc = "currentt line blame" })
map("n", "<leader>gb", function() snacks.gitbrowse() end, { desc = "open current github" })

local function commit_files()
  local message = vim.fn.input("Commit message: ")
  vim.cmd("Git add .")
  vim.cmd('Git commit -m "' .. message .. '"')
end
map("n", "<leader>gc", commit_files, { desc = "Git commit current file" })
map({ "n", "v" }, "<leader>gH", function()
  require("gitsigns").stage_hunk({ vim.fn.line("."), vim.fn.line("v") })
end, { desc = "Stage hunk" })
-- map("n", "<leader>ghs", ":Gitsign stage_buffer<CR>", { desc = "stage hunk" })
-- map("n", "<leader>ghh", ":Gitsign preview_hunk<CR>", { desc = "Preview hunk" })
-- map({ "n", "v" }, "<leader>ghr", function()
--   require("gitsigns").reset_hunk({ vim.fn.line("."), vim.fn.line("v") })
-- end, { desc = "git reset hunk" })
-- map("n", "<leader>gss", ":Gitsign stage_hunk<CR>", { desc = "stage hunk" })
-- map("n", "<leader>ghr", ":Gitsign reset_hunk<CR>", { desc = "reset hunk" })

-- local autocmd = vim.api.nvim_create_autocmd
-- autocmd("BufWinEnter", {
--   group = vim.api.nvim_create_augroup("wt/fugitive", {}),
--   pattern = "*",
--   callback = function()
--     if vim.bo.ft ~= "fugitive" then
--       return
--     end
--     local bufnr = vim.api.nvim_get_current_buf()
--     local opts = { buffer = bufnr, remap = false }
--     vim.keymap.set("n", "<leader>p", function()
--       vim.cmd.Git("push")
--     end, opts)
--     -- rebase always
--     vim.keymap.set("n", "<leader>P", function()
--       vim.cmd.Git({ "pull", "--rebase" })
--     end, opts)
--     vim.keymap.set("n", "<leader>t", ":Git push -u origin ", opts)
--   end,
-- })
-- map("n", "gu", "<cmd>diffget //2<CR>", { desc = "Diff get" })
-- map("n", "gh", "<cmd>diffget //3<CR>", { desc = "Diff get" })
