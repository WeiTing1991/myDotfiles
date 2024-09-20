-- https://github.com/nvim-telescope/telescope.nvim/wiki/Configuration-Recipes#pickers

local builtin = require "telescope.builtin"
local utils = require "telescope.utils"
local actions = require "telescope.actions"

-- setup
require("telescope").setup {
  defaults = {
    vimgrep_arguments = {
      "rg",
      "-L",
      "--hidden",
      "--glob",
      "!**/.git/*",
      "--smart-case",
      "--with-filename",
      '--column',

      '--line-number',
    },
    -- path_display = "smart",
    initial_mode = "insert",
    layout_config = {
      horizontal = {
        prompt_position = "bottom",
        preview_width = 0.65,
      },
      vertical = {
        mirror = false,
      },
      width = 0.85,
      height = 0.90,
      preview_cutoff = 80,
    },
    mappings = {
      n = {
        ["<C-p>"] = actions.move_selection_previous, -- move to prev result
        ["<C-n>"] = actions.move_selection_next,     -- move to next result
        ["<C-d>"] = actions.delete_buffer,
        ["<C-q>"] = actions.send_selected_to_qflist + actions.open_qflist,
        ["q"] = actions.close,
      },
    },
  },
  pickers = {
    find_files = {
      hidden = true,
      find_command = {
        "rg",
        "-L",
        "--files",
        "--hidden",
        "--glob",
        "!**/.git/*",
      },
    },
    grep_string = {
      additional_args = { "--hidden" }
    },
    live_grep = {
      additional_args = { "--hidden" }
    },
  },
  extensions = {
    fzf = {
      fuzzy = true,                   -- false will only do exact matching
      override_generic_sorter = true, -- override the generic sorter
      override_file_sorter = true,    -- override the file sorter
      case_mode = "smart_case",       -- or "ignore_case" or "respect_case"
    },
  },
}

pcall(require("telescope").load_extension, "fzf")

-- keymaps
vim.keymap.set("n", "<leader>f", builtin.find_files, { desc = "Find Files" })
vim.keymap.set("n", "<leader>sk", builtin.keymaps, { desc = "Search Keymaps" })
vim.keymap.set("n", "<leader><leader>", builtin.buffers, { desc = "Find file in buffer" })
vim.keymap.set("n", "<space>ff", function() builtin.find_files { cwd = utils.buffer_dir() } end,
  { desc = "file browser in buffer" })
vim.keymap.set("n", "<leader>fl", function() builtin.live_grep { grep_open_files = true } end,
  { desc = "Find live grep" })

vim.keymap.set("n", "<leader>fs", function()
  builtin.grep_string { search = vim.fn.input "Grep > " }
end, { desc = "Grep search" })
--
-- word search
vim.keymap.set("n", "<leader>fw", function()
  local word = vim.fn.expand "<cword>"
  builtin.grep_string { search = word }
end, { desc = "word search" })
vim.keymap.set("n", "<leader>fW", function()
  local word = vim.fn.expand "<cWORD>"
  builtin.grep_string { search = word }
end, { desc = "cWord search" })
