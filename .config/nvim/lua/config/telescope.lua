local actions = require "telescope.actions"
local builtin = require "telescope.builtin"
-- :hlep check this https://github.com/nvim-telescope/telescope.nvim/wiki/Configuration-Recipes#remove--from-fd-results
require("telescope").setup {
  defaults = {
    vimgrep_arguments = {
      "rg",
      "-L",
      "--color=never",
      "--no-heading",
      "--with-filename",
      "--line-number",
      "--column",
      "--smart-case",
      "--hidden",
      "--glob",
      "!**/.git/*",
    },
    prompt_prefix = " / ",
    selection_caret = "  ",
    entry_prefix = "  ",
    initial_mode = "normal",
    selection_strategy = "reset",
    sorting_strategy = "ascending",
    layout_strategy = "horizontal",
    layout_config = {
      horizontal = {
        prompt_position = "top",
        preview_width = 0.65,
      },
      vertical = {
        mirror = false,
      },
      width = 0.85,
      height = 0.90,
      preview_cutoff = 80,
    },
    file_sorter = require("telescope.sorters").get_fuzzy_file,
    file_ignore_patterns = { "node_modules" },
    generic_sorter = require("telescope.sorters").get_generic_fuzzy_sorter,
    path_display = { "truncate" },
    winblend = 0,
    border = {},
    borderchars = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
    color_devicons = true,
    set_env = { ["COLORTERM"] = "truecolor" }, -- default = nil,
    file_previewer = require("telescope.previewers").vim_buffer_cat.new,
    grep_previewer = require("telescope.previewers").vim_buffer_vimgrep.new,
    qflist_previewer = require("telescope.previewers").vim_buffer_qflist.new,
    -- --Developer configurations: Not meant for general override
    -- buffer_previewer_maker = require('telescope.previewers').buffer_previewer_maker,
    mappings = {
      n = {
        ["<C-p>"] = actions.move_selection_previous, -- move to prev result
        ["<C-n>"] = actions.move_selection_next, -- move to next result
        ["<C-d>"] = actions.delete_buffer,
        ["<C-q>"] = actions.send_selected_to_qflist + actions.open_qflist,
        ["q"] = require("telescope.actions").close,
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
  },
  extensions = {
    ["ui-select"] = {
      require("telescope.themes").get_dropdown {},
    },
    fzf = {
      fuzzy = true, -- false will only do exact matching
      override_generic_sorter = true, -- override the generic sorter
      override_file_sorter = true, -- override the file sorter
      case_mode = "smart_case", -- or "ignore_case" or "respect_case"
    },
    file_browser = {
      theme = "ivy",
      -- disables netrw and use telescope-file-browser in its place
      --hijack_netrw = true,
      mappings = {
        ["i"] = {
          -- your custom insert mode mappings
        },
        ["n"] = {
          -- your custom normal mode mappings
        },
      },
    },
  },
  extensions_list = { "themes", "terms" },
}

pcall(require("telescope").load_extension, "fzf")
pcall(require("telescope").load_extension, "file_browser")
pcall(require("telescope").load_extension, "ui-select")

-- keymapping
vim.keymap.set("n", "<space>fB", ":Telescope file_browser<CR>", { desc = "File browser" })
vim.keymap.set(
  "n",
  "<space>fb",
  ":Telescope file_browser path=%:p:h select_buffer=true<CR>",
  { desc = "file browser in buffer" }
)

-- open the nvim config file folder
vim.keymap.set(
  "n",
  "<space>fc",
  ":Telescope file_browser path=" .. vim.fn.stdpath "config" .. "<CR>",
  { desc = "open the nvim config" }
)
vim.keymap.set(
  "n",
  "<space>fo",
  ":Telescope file_browser path=" .. vim.fn.expand ("~/.local/share/nvim/lazy/rose-pine.nvim") .. "<CR>",
  { desc = "open the colorscheme config" }
)

--fzf keybinding
vim.keymap.set("n", "<leader>ky", builtin.keymaps, { desc = " Keymaps" })
vim.keymap.set("n", "<leader>f", builtin.find_files, { desc = "Find Files" })
vim.keymap.set("n", "<leader>fg", builtin.git_files, { desc = "Find Git Files" })
vim.keymap.set("n", "<leader>fh", builtin.help_tags, { desc = "Help Teles" })
vim.keymap.set("n", "<leader><leader>", builtin.buffers, { desc = "Find file in buffer" })
--vim.keymap.set("n", "<leader>fb", "<cmd>Telescope current_buffer_fuzzy_find sorting_strategy=ascending prompt_position=top<CR>" )
vim.keymap.set("n", "<leader>fl", builtin.live_grep, { desc = "Find live grep" })

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
