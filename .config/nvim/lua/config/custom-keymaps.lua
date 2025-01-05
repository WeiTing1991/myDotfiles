--------------------------- Pluglins keymaps ---------------------------------
-- NOTE: https://github.com/echasnovski/mini.nvim/blob/main/doc/mini-clue.txt

local miniclue = require "mini.clue"
miniclue.setup {
  triggers = {
    -- Leader triggers
    { mode = "n", keys = "<Leader>" },
    { mode = "x", keys = "<Leader>" },

    -- Leader 2 triggers
    { mode = "n", keys = "<C-l>" },
    { mode = "x", keys = "<C-l>" },

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
-- File tree
vim.keymap.set("n", "<leader>d", function()
  require("oil").open_float()
end, { desc = "Toggle file explorer" })

-- Markdown
vim.keymap.set("n", "<leader>mm", "<cmd>MarkdownPreviewToggle<cr>", { desc = "markdown toggle" })
vim.keymap.set("n", "<leader>mp", "<cmd>MarkdownPreview<cr>", { desc = "markdown preview" })
vim.keymap.set("n", "<leader>mk", "<cmd>MarkdownPreviewStop<cr>", { desc = "markdown stop" })
vim.keymap.set("n", "<leader>mr", "<cmd>Markview<cr>", { desc = "markdown render toggle" })

-- Zen mode
vim.keymap.set("n", "<leader>z", function()
  require("zen-mode").toggle { window = { width = 0.85 } }
end, { desc = "Zen mode" })

-- gitsigns
vim.keymap.set("n", "<leader>gh", ":Gitsign preview_hunk<CR>", { desc = "Preview hunk" })
vim.keymap.set("n", "<leader>gb", ":Gitsign toggle_current_line_blame<CR>", { desc = "currentt line blame" })

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

-- trouble
vim.keymap.set("n", "<leader>td", "<cmd>Trouble diagnostics toggle<cr>", { desc = "Diagnostics (Trouble)" })

-- undotree
-- vim.keymap.set("n", "<leader>u", vim.cmd.UndotreeToggle, { desc = "Undotree" })
