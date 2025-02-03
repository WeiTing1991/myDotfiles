---------------------------------- Pluglins keymaps ---------------------------------
-- NOTE: https://github.com/echasnovski/mini.nvim/blob/main/doc/mini-clue.txt

local miniclue = require('mini.clue')
miniclue.setup({
  triggers = {
    -- Leader triggers
    { mode = "n", keys = "<Leader>" },
    { mode = "x", keys = "<Leader>" },

    -- Leader 2 triggers
    { mode = "n", keys = "<S-l>" },
    { mode = "x", keys = "<S-l>" },

    -- Leader 2 triggers
    { mode = "n", keys = "<C-g>" },
    { mode = "x", keys = "<C-g>" },

    -- Built-in completion
    -- { mode = "i", keys = "<C-x>" },

    -- `g` key
    -- { mode = "n", keys = "g" },
    -- { mode = "x", keys = "g" },

    -- Marks
    -- { mode = "n", keys = "'" },
    -- { mode = "n", keys = "`" },
    -- { mode = "x", keys = "'" },
    -- { mode = "x", keys = "`" },
    --
    -- Registers
    -- { mode = "n", keys = "\"" },
    -- { mode = "x", keys = "\"" },
    -- { mode = "i", keys = "<C-r>" },
    -- { mode = "c", keys = "<C-r>" },

    -- Window commands
    { mode = "n", keys = "<C-w>" },

    -- `z` key
    { mode = "n", keys = "z" },
    { mode = "x", keys = "z" },
  },

  clues = {
    -- Enhance this by adding descriptions for <Leader> mapping groups
    miniclue.gen_clues.builtin_completion(),
    miniclue.gen_clues.g(),
    -- miniclue.gen_clues.marks(),
    -- miniclue.gen_clues.registers(),
    miniclue.gen_clues.windows(),
    miniclue.gen_clues.z(),
  },
  window = {
    delay = 0,
    config = {},
    scroll_down = "<C-d>",
    scroll_up = "<C-u>",
  },
})

---------------------------------- Pluglins keymaps ---------------------------------
local map = vim.keymap.set

-- fzf
-- check https://github.com/nvim-telescope/telescope.nvim?tab=readme-ov-file#pickers
local builtin = require "telescope.builtin"

map("n", "<C-f>", builtin.find_files, { desc = "Find files" })
map("n", "<leader>ff", builtin.find_files, { desc = "Find files" })
map("n", "<leader><leader>", function()
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

-- theme switcher
map("n", "<leader>th", ":lua require('base46').toggle_theme()<cr>", { desc = " Switch Themes" })

-- File tree
map("n", "<leader>d", function()
  require("oil").open_float()
end, { desc = "Toggle file explorer" })

-- Markdown
map("n", "<leader>mm", "<cmd>PeekOpen<cr>", { desc = "markdown preview with deno" })
map("n", "<leader>mk", "<cmd>PeekClose<cr>", { desc = "markdown stop" })
map("n", "<leader>mr", "<cmd>Markview<cr>", { desc = "markdown render toggle" })
map("n", "<leader>mp", "<cmd>MarkdownPreviewToggle<cr>", { desc = "markdown preview with node" })

-- Tree
map("n", "<C-e>", "<cmd>NvimTreeToggle<cr>", { desc = "File tree" })

-- windows
map("n", "<leader>'", function()
  require("custom_plugins.toggle_maximize_window").toggle_maximize_window()
end, { desc = "Toggle maximize buffer" })

-- git tools
-- https://www.youtube.com/watch?v=IyBAuDPzdFY&t=22s
-- https://www.naseraleisa.com/posts/diff
map("v", "<C-g>s", function()
  require("gitsigns").stage_hunk { vim.fn.line ".", vim.fn.line "v" }
end, { desc = "git stage hunk" })
map("v", "<C-g>r", function()
  require("gitsigns").reset_hunk { vim.fn.line ".", vim.fn.line "v" }
end, { desc = "git reset hunk" })

map("n", "<C-g>h", ":Gitsign preview_hunk<CR>", { desc = "Preview hunk" })
map("n", "<C-g>s", ":Gitsign stage_hunk<CR>", { desc = "stage hunk" })

map("n", "<C-g>S", ":Gitsign stage_buffer<CR>", { desc = "stage hunk" })
map("n", "<C-g>r", ":Gitsign reset_hunk<CR>", { desc = "reset hunk" })
map("n", "<C-g>i", ":Gitsign toggle_current_line_blame<CR>", { desc = "currentt line blame" })

-- map("n", "<C-g>d", function()
--   require('gitsigns').diffthis '@'
-- end, { desc = "git diff last commit" })
-- map("n", "<C-g>d", function()
--   require('gitsigns').diffthis()
-- end, { desc = "git diff" })

map("n", "<C-g>g", vim.cmd.Git, { desc = "Git" })
map("n", "<C-g>gp", function()
  vim.cmd.Git ("push")
end, { desc = "Git push" })
map("n", "<C-g>gP", function()
  vim.cmd.Git ({"pull", "--rebase"})
end, { desc = "Git push" })

map("n", "<C-g>gP", function()
  vim.cmd.Git ({"pull", "--rebase"})
end, { desc = "Git pull rebase " })

local function commit_current_file()
  local file = vim.fn.expand('%')
  local message = vim.fn.input('Commit message: ')
  vim.cmd('Git add ' .. file)
  vim.cmd('Git commit -m "' .. message .. '"')
end
map('n', '<C-g>ca', commit_current_file, { desc = "Git commit current file" })

-- toggle copilot
map("n", "<leader>tc", function()
  require("copilot.suggestion").toggle_auto_trigger()
  if not vim.b.copilot_suggestion_auto_trigger then
    print "Copilot is disabled"
  else
    print "Copilot is enabled"
  end
end, { desc = "Copilot" })

-- map("n", "<leader>td", vim.diagnostic.hide, { desc = "Hide diagnostics" })

-- Zen mode
-- map("n", "<leader>tz", function()
--   require("zen-mode").toggle { window = { width = 0.85 } }
-- end, { desc = "Zen mode" })

-- trouble
map("n", "<leader>td", "<cmd>Trouble diagnostics toggle<cr>", { desc = "Diagnostics (Trouble)" })

-- trouble
map("n", "<leader>ta", "<cmd>Neogen<cr>", { desc = "Add Annotation" })

-- undotree
-- map("n", "<leader>u", vim.cmd.UndotreeToggle, { desc = "Undotree" })

-- map("n", "<C-g>", builtin.git_files, { desc = "Find live grep" })

-- -- word search
-- map("n", "<leader>fw", function()
--   local word = vim.fn.expand "<cword>"
--   builtin.grep_string { search = word }
-- end, { desc = "word search" })

-- map("n", "<leader>fW", function()
--   local word = vim.fn.expand "<cWORD>"
--   builtin.grep_string { search = word }
-- end, { desc = "cWord search" })
