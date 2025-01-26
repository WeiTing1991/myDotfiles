--------------------------- Pluglins keymaps ---------------------------------
-- NOTE: https://github.com/echasnovski/mini.nvim/blob/main/doc/mini-clue.txt

local miniclue = require "mini.clue"
miniclue.setup {
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
    config = { width = 60 },
    scroll_down = "<C-d>",
    scroll_up = "<C-u>",
  },
}

-- --------------------------- Pluglins keymaps ---------------------------------
-- theme switcher
vim.keymap.set("n", "<leader>th", ":lua require('base46').toggle_theme()<cr>",
  { desc = "Themes" })

-- File tree
vim.keymap.set("n", "<leader>d", function()
  require("oil").open_float()
end, { desc = "Toggle file explorer" })

-- Markdown
vim.keymap.set("n", "<leader>mm", "<cmd>PeekOpen<cr>", { desc = "markdown preview" })
vim.keymap.set("n", "<leader>mk", "<cmd>PeekClose<cr>", { desc = "markdown stop" })
vim.keymap.set("n", "<leader>mr", "<cmd>Markview<cr>", { desc = "markdown render toggle" })

-- Tree
vim.keymap.set("n", "<C-e>", "<cmd>NvimTreeToggle<cr>", { desc = "File tree" })

-- git tools
-- https://www.youtube.com/watch?v=IyBAuDPzdFY&t=22s
-- https://www.naseraleisa.com/posts/diff
vim.keymap.set('v', '<C-g>s', function()
  require('gitsigns').stage_hunk { vim.fn.line '.', vim.fn.line 'v' }
end, { desc = 'git stage hunk' })
vim.keymap.set('v', '<C-g>r', function()
  require('gitsigns').reset_hunk { vim.fn.line '.', vim.fn.line 'v' }
end, { desc = 'git reset hunk' })

vim.keymap.set("n", "<C-g>h", ":Gitsign preview_hunk<CR>", { desc = "Preview hunk" })
vim.keymap.set("n", "<C-g>s", ":Gitsign stage_hunk<CR>", { desc = "stage hunk" })

vim.keymap.set("n", "<C-g>S", ":Gitsign stage_buffer<CR>", { desc = "stage hunk" })
vim.keymap.set("n", "<C-g>r", ":Gitsign reset_hunk<CR>", { desc = "reset hunk" })
vim.keymap.set("n", "<C-g>i", ":Gitsign toggle_current_line_blame<CR>", { desc = "currentt line blame" })

-- vim.keymap.set("n", "<C-g>d", function()
--   require('gitsigns').diffthis '@'
-- end, { desc = "git diff last commit" })
-- vim.keymap.set("n", "<C-g>d", function()
--   require('gitsigns').diffthis()
-- end, { desc = "git diff" })


-- toggle copilot
vim.keymap.set("n", "<leader>tc", function()
  require("copilot.suggestion").toggle_auto_trigger()
  if not vim.b.copilot_suggestion_auto_trigger then
    print "Copilot is disabled"
  else
    print "Copilot is enabled"
  end
end, { desc = "Copilot" })

-- vim.keymap.set("n", "<leader>td", vim.diagnostic.hide, { desc = "Hide diagnostics" })

-- Zen mode
vim.keymap.set("n", "<leader>tz", function()
  require("zen-mode").toggle { window = { width = 0.85 } }
end, { desc = "Zen mode" })

-- trouble
vim.keymap.set("n", "<leader>td", "<cmd>Trouble diagnostics toggle<cr>", { desc = "Diagnostics (Trouble)" })

-- trouble
vim.keymap.set("n", "<leader>ta", "<cmd>Neogen<cr>", { desc = "Add Annotation" })

-- undotree
-- vim.keymap.set("n", "<leader>u", vim.cmd.UndotreeToggle, { desc = "Undotree" })
