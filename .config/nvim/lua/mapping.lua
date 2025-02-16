---------------------------------- Plugins Keymaps ---------------------------------
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
local fzf = require("fzf-lua")

-- fzf
map("n", "<C-f>", fzf.files, { desc = "Find files" })
map("n", "<leader>ff", fzf.files, { desc = "Find files" })
map("n", "<leader><leader>", function()
  require("fzf-lua").files({ cwd = vim.fn.expand("%:p:h") })
end, { desc = "Finde file in current child dir" })
map("n", "<leader>fo", fzf.oldfiles, { desc = "Open recent file" })
map("n", "<leader>fb", fzf.buffers, { desc = "Finde file in current child dir" })

map("n", "<leader>fk", fzf.keymaps, { desc = "Search keymaps" })
map("n", "<leader>fg", function() fzf.grep({ search = vim.fn.input "Grep > " }) end, { desc = "Grep search" })
map("n", "<leader>fl", fzf.live_grep, { desc = "Find live grep" })

map("n", "<leader>bb", fzf.tabs, { desc = "Find live grep" })
map("n", "<leader>bt", fzf.tmux_buffers, { desc = "Find live grep" })

-- theme switcher
map("n", "<leader>th", ":lua require('base46').toggle_theme()<cr>", { desc = " Switch Themes" })

-- File tree
map("n", "<leader>d", function() require("oil").open_float() end, { desc = "Toggle file explorer" })

-- Markdown
map("n", "<leader>mm", "<cmd>PeekOpen<cr>", { desc = "markdown preview with deno" })
map("n", "<leader>mk", "<cmd>PeekClose<cr>", { desc = "markdown stop" })
map("n", "<leader>mr", "<cmd>Markview<cr>", { desc = "markdown render toggle" })
map("n", "<leader>mp", "<cmd>MarkdownPreviewToggle<cr>", { desc = "markdown preview with node" })

-- Bold (Ctrl + b)
map("v", "<leader>mb", "c**<C-r>\"**<Esc>", { desc = "Bold" })
-- Italic (Ctrl + i)
map("v", "<leader>mi", "c*<C-r>\"*<Esc>", { desc = "Italic" })
-- Inline Code (Ctrl + `)
map("v", "<leader>m`", "c`<C-r>\"`<Esc>", { desc = "Inline Code" })
-- Strikethrough (Ctrl + s)
map("v", "<leader>ms", "c~~<C-r>\"~~<Esc>", { desc = "Strikethrough" })
-- Code block (Ctrl + )
map("v", "<leader>mc", "c```<C-r>/```<Esc>", { desc = "Strikethrough" })

-- Tree
map("n", "<C-e>", "<cmd>NvimTreeToggle<cr>", { desc = "File tree" })

-- windows
map("n", "<leader>'", function()
  require("custom_plugins.toggle_maximize_window").toggle_maximize_window()
end, { desc = "Toggle maximize buffer" })

-- git tools
-- https://www.youtube.com/watch?v=IyBAuDPzdFY&t=22s
-- https://www.naseraleisa.com/posts/diff
map("v", "<C-g>ss", function()
  require("gitsigns").stage_hunk { vim.fn.line ".", vim.fn.line "v" }
end, { desc = "git stage hunk" })
map("n", "<C-g>ss", ":Gitsign stage_hunk<CR>", { desc = "stage hunk" })

map("v", "<C-g>r", function()
  require("gitsigns").reset_hunk { vim.fn.line ".", vim.fn.line "v" }
end, { desc = "git reset hunk" })

map("n", "<C-g>st", ":FzfLua git_status<CR>", { desc = "Status" })
map("n", "<C-g>b", ":FzfLua git_branches<CR>", { desc = "Branch" })
map("n", "<C-g>cb", ":FzfLua git_bcommits<CR>", { desc = "Buffer Commits" })
map("n", "<C-g>cc", ":FzfLua git_commits<CR>", { desc = "Commits" })

map("n", "<C-g>h", ":Gitsign preview_hunk<CR>", { desc = "Preview hunk" })

map("n", "<C-g>S", ":Gitsign stage_buffer<CR>", { desc = "stage hunk" })
map("n", "<C-g>r", ":Gitsign reset_hunk<CR>", { desc = "reset hunk" })
map("n", "<C-g>i", ":Gitsign toggle_current_line_blame<CR>", { desc = "currentt line blame" })

-- map("n", "<C-g>gg", vim.cmd.Git, { desc = "Git" })
map("n", "<C-g>gP", function() vim.cmd.Git("push") end, { desc = "Git push" })
-- map("n", "<C-g>gp", function() vim.cmd.Git({ "pull", "--rebase" }) end, { desc = "Git push" })
map("n", "<C-g>gpr", function() vim.cmd.Git({ "pull", "--rebase" }) end, { desc = "Git pull rebase " })

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

map('n', '<C-g>gd', ":DiffviewOpen<cr>", { desc = "Git Diff" })
map('n', '<C-g>q', ":DiffviewClose<cr>", { desc = "Close Git Diff" })

-- CHECK: https://github.com/ibhagwan/fzf-lua
-- and trouble

-- diagnostics
-- map("n", "<leader>td", vim.diagnostic.hide, { desc = "Hide diagnostics" })
map("n", "<leader>td", ":FzfLua diagnostics_document<cr>", { desc = "diagnostics" })
map("n", "<leader>tD", ":FzfLua diagnostics_workspace<cr>", { desc = "diagnostics workspace" })

-- trouble
-- map("n", "<leader>td", "<cmd>Trouble diagnostics toggle<cr>", { desc = "Diagnostics (Trouble)" })

-- Annotations
map("n", "<leader>ta", "<cmd>Neogen<cr>", { desc = "Annotation" })

-- undotree
-- map("n", "<leader>u", vim.cmd.UndotreeToggle, { desc = "Undotree" })

-- Zen mode
-- map("n", "<leader>tz", function()
--   require("zen-mode").toggle { window = { width = 0.85 } }
-- end, { desc = "Zen mode" })


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
