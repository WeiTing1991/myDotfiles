local tele = require("telescope")

-- setup
tele.setup({
  -- defaults = {
  --   -- vimgrep_arguments = {
  --   --   "rg",
  --   --   "-L",
  --   --   "--color=never",
  --   --   "--no-heading",
  --   --   "--with-filename",
  --   --   "--line-number",
  --   --   "--column",
  --   --   "--smart-case",
  --   --   "--hidden",
  --   --   "--glob",
  --   --   "!**/.git/*",
  --   -- },
  --   path_display = { "smart" },
  --   initial_mode = "normal",
  --   layout_config = {
  --     horizontal = {
  --       prompt_position = "bottom",
  --       preview_width = 0.7,
  --     },
  --     -- vertical = {
  --     --   mirror = true,
  --     -- },
  --     width = 0.95,
  --     height = 0.95,
  --     -- preview_cutoff = 50,
  --   },
  --   mappings = {
  --     n = {
  --       ["<C-p>"] = actions.move_selection_previous,
  --       ["<C-n>"] = actions.move_selection_next,
  --       ["d"] = actions.delete_buffer,
  --       -- ["<C-q>"] = actions.send_selected_to_qflist + actions.open_qflist,
  --       ["q"] = actions.close,
  --       ["<C-o>"] = function(prompt_bufnr)
  --         local selection = require('telescope.actions.state').get_selected_entry()
  --         require('telescope.actions').close(prompt_bufnr)
  --         if selection ~= nil then
  --           vim.cmd('vsplit ' .. selection.value) -- Opens in vertical split
  --         end
  --       end,
  --     },
  --     i = {
  --       ["<C-o>"] = function(prompt_bufnr)
  --         local selection = require('telescope.actions.state').get_selected_entry()
  --         require('telescope.actions').close(prompt_bufnr)
  --         if selection ~= nil then
  --           vim.cmd('vsplit ' .. selection.value) -- Opens in vertical split
  --         end
  --       end,
  --     },
  --   },
  -- },
  pickers = {
    spell_suggest = {
      theme = "cursor",
      previewer = false,
    },
    --   find_files = {
    --     -- theme = "ivy",
    --     find_command = {
    --       "fd",
    --       "--strip-cwd-prefix",
    --       "--type", "f", -- Only find files
    --       "--hidden",
    --       "--exclude",
    --       ".git",
    --       "--exclude",
    --       ".github",
    --       "--exclude",
    --       "node_modules",
    --       "--exclude",
    --       ".venv",
    --       "--exclude=**/.DS_Store/**",
    --       "--exclude=.DS_Store",
    --     },
    --   },
    --   grep_string = {
    --     -- theme = "ivy",
    --     additional_args = {
    --       "-L",
    --       "--hidden",
    --       "--glob=!**/.git/*",
    --       "--glob=!**/node_modules/*",
    --       -- "!**/.github/*",
    --       "--smart-case",
    --     },
    --   },
    --   live_grep = {
    --     -- theme = "ivy",
    --     additional_args = {
    --       "-L",
    --       "--hidden",
    --       "--glob=!**/.git/*",
    --       "--glob=!**/node_modules/*",
    --       -- "!**/.github/*",
    --       "--smart-case",
    --     },
    --   },
  },
  extensions = {
    fzf = {
      fuzzy = true,
      override_generic_sorter = true,
      override_file_sorter = true,
      case_mode = "smart_case",
    },
    frecency = {
      show_scores = false,
      db_safe_mode = false,
      auto_validate = true,
      db_validate_threshold = 10,
      show_filter_column = false,
      show_unindexed = true,
      default_workspace = "CWD",
    },
    ["ui-select"] = {
      require("telescope.themes").get_cursor({
        layout_config = {
          width = 0.65,
          height = 0.25,
        },
        previewer = false,
        prompt_title = "  Select an action",
        results_title = "",
        sotrting_strategy = "ascending",
      }),
    },
  },
})

pcall(tele.load_extension, "fzf")
pcall(tele.load_extension, "frecency")
pcall(tele.load_extension, "ui-select")
