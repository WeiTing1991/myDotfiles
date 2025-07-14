----- Pluglins keymaps -----
local map = vim.keymap.set
local tele_builtin = require("telescope.builtin")

--[[ file tree ]]
map("n", "<leader>d", function() require("oil").open() end, { desc = "Toggle file explorer" })

--[[ telescope/search ]]
map("n", "<C-f>", tele_builtin.find_files, { desc = "Find files" })
map("n", "<leader>ff", tele_builtin.find_files, { desc = "Find files" })
map("n", "<leader>fl", tele_builtin.live_grep, { desc = "Find live grep" })
map("n", "<leader>fo", tele_builtin.oldfiles, { desc = "Open recent file" })
map("n", "<leader>fb", tele_builtin.buffers, { desc = "Find file in opened buffer" })
-- map("n", "<leader>fj", function()
--   builtin.find_files({ cwd = vim.fn.expand("%:p:h") })
-- end, { desc = "Find file in current child dir" })
-- map("n", "<leader>fg", function()
--   local search_term = vim.fn.input("Grep > ")
--   if search_term ~= "" then
--     builtin.live_grep({ default_text = search_term })
--   end
-- end, { desc = "Grep search" })
-- map("n", "<leader>fk", builtin.keymaps, { desc = "Search keymaps" })
-- -- Note: telescope doesn't have direct tabs equivalent, using buffers instead
-- map("n", "<leader>ft", builtin.buffers, { desc = "Find buffers (tabs alternative)" })
-- -- Note: telescope doesn't have tmux_buffers, you might need a plugin or custom function
-- -- map("n", "<leader>ftb", builtin.tmux_buffers, { desc = "Find tmux buffer" })
-- map("n", "z=", builtin.spell_suggest, { desc = "Spelling suggestions" })
--
-- -- Your existing telescope keymaps (keeping for reference)
-- map('n', '<leader>sh', builtin.help_tags, { desc = '[S]earch [H]elp' })
-- map('n', '<leader>sk', builtin.keymaps, { desc = '[S]earch [K]eymaps' })
-- map('n', '<leader>sf', builtin.find_files, { desc = '[S]earch [F]iles' })
-- map('n', '<leader>ss', builtin.builtin, { desc = '[S]earch [S]elect Telescope' })
-- map('n', '<leader>sw', builtin.grep_string, { desc = '[S]earch current [W]ord' })
-- map('n', '<leader>sg', builtin.live_grep, { desc = '[S]earch by [G]rep' })
-- map('n', '<leader>sd', builtin.diagnostics, { desc = '[S]earch [D]iagnostics' })
-- map('n', '<leader>sr', builtin.resume, { desc = '[S]earch [R]esume' })
-- map('n', '<leader>s.', builtin.oldfiles, { desc = '[S]earch Recent Files ("." for repeat)' })
-- map('n', '<leader><leader>', builtin.buffers, { desc = '[ ] Find existing buffers' })
--
-- map('n', '<leader>/', function()
--   builtin.current_buffer_fuzzy_find(require('telescope.themes').get_dropdown {
--     winblend = 10,
--     previewer = false,
--   })
-- end, { desc = '[/] Fuzzily search in current buffer' })
--
-- map('n', '<leader>s/', function()
--   builtin.live_grep {
--     grep_open_files = true,
--     prompt_title = 'Live Grep in Open Files',
--   }
-- end, { desc = '[S]earch [/] in Open Files' })
--
-- map('n', '<leader>sn', function()
--   builtin.find_files { cwd = vim.fn.stdpath 'config' }
-- end, { desc = '[S]earch [N]eovim files' })
