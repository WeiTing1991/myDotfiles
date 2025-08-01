-- telescope config
-- local builtin = require "telescope.builtin"
-- local utils = require "telescope.utils"
local actions = require "telescope.actions"
local tele = require("telescope")

-- setup
tele.setup {
  defaults = {
    -- vimgrep_arguments = {
    --   "rg",
    --   "-L",
    --   "--color=never",
    --   "--no-heading",
    --   "--with-filename",
    --   "--line-number",
    --   "--column",
    --   "--smart-case",
    --   "--hidden",
    --   "--glob",
    --   "!**/.git/*",
    -- },
    path_display = { "smart" },
    initial_mode = "normal",
    layout_config = {
      horizontal = {
        prompt_position = "bottom",
        preview_width = 0.7,
      },
      -- vertical = {
      --   mirror = true,
      -- },
      width = 0.95,
      height = 0.95,
      -- preview_cutoff = 50,
    },
    mappings = {
      n = {
        ["<C-p>"] = actions.move_selection_previous,
        ["<C-n>"] = actions.move_selection_next,
        ["d"] = actions.delete_buffer,
        -- ["<C-q>"] = actions.send_selected_to_qflist + actions.open_qflist,
        ["q"] = actions.close,
        ["<C-o>"] = function(prompt_bufnr)
          local selection = require('telescope.actions.state').get_selected_entry()
          require('telescope.actions').close(prompt_bufnr)
          if selection ~= nil then
            vim.cmd('vsplit ' .. selection.value) -- Opens in vertical split
          end
        end,
      },
      i = {
        ["<C-o>"] = function(prompt_bufnr)
          local selection = require('telescope.actions.state').get_selected_entry()
          require('telescope.actions').close(prompt_bufnr)
          if selection ~= nil then
            vim.cmd('vsplit ' .. selection.value) -- Opens in vertical split
          end
        end,
      },
    },
  },
  pickers = {
    find_files = {
      -- theme = "ivy",
      find_command = {
        "fd",
        "--strip-cwd-prefix",
        "--type", "f", -- Only find files
        "--hidden",
        "--exclude",
        ".git",
        "--exclude",
        ".github",
        "--exclude",
        "node_modules",
        "--exclude",
        ".venv",
        "--exclude=**/.DS_Store/**",
        "--exclude=.DS_Store",
      },
    },
    grep_string = {
      -- theme = "ivy",
      additional_args = {
        "-L",
        "--hidden",
        "--glob=!**/.git/*",
        "--glob=!**/node_modules/*",
        -- "!**/.github/*",
        "--smart-case",
      },
    },
    live_grep = {
      -- theme = "ivy",
      additional_args = {
        "-L",
        "--hidden",
        "--glob=!**/.git/*",
        "--glob=!**/node_modules/*",
        -- "!**/.github/*",
        "--smart-case",
      },
    },
  },
  extensions = {
    fzf = {
      fuzzy = true,                   -- false will only do exact matching
      override_generic_sorter = true, -- override the generic sorter
      override_file_sorter = true,    -- override the file sorter
      case_mode = "smart_case",       -- or "ignore_case" or "respect_case"
    },
    frecency = {
      show_scores = true,         -- Default: false
      db_safe_mode = false,       -- Default: true
      auto_validate = true,       -- Default: true
      db_validate_threshold = 10, -- Default: 10
      show_filter_column = false, -- Default: true
    },
   ["ui-select"] = {
      require("telescope.themes").get_dropdown({ }) }
  },
}

pcall(tele.load_extension, "fzf")
pcall(tele.load_extension, "frecency")
pcall(tele.load_extension, "ui-select")
