local options = {}

return {

  "nvim-telescope/telescope.nvim",
  event = "VeryLazy",
  branch = "0.1.x",
  dependencies = {
    "nvim-lua/plenary.nvim",
    {
      "nvim-telescope/telescope-fzf-native.nvim",
      build = "make",
    },
    { "nvim-telescope/telescope-file-browser.nvim" },
    { "nvim-telescope/telescope-ui-select.nvim" },
    { "nvim-tree/nvim-web-devicons" },
  },
  config = function()
    local actions = require("telescope.actions")
    local builtin = require("telescope.builtin")
    local fb_actions = require("telescope._extensions.file_browser.actions")
    require("telescope").setup({
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
        },
        pickers = {
          find_files = {
            find_command = {
              theme = "dropdown",
            },
          },
        },
        prompt_prefix = " ? ",
        selection_caret = "  ",
        entry_prefix = "  ",
        initial_mode = "normal",
        selection_strategy = "reset",
        sorting_strategy = "ascending",
        layout_strategy = "horizontal",
        layout_config = {
          horizontal = {
            prompt_position = "top",
            preview_width = 0.60,
          },
          vertical = {
            mirror = false,
          },
          width = 0.75,
          height = 0.85,
          preview_cutoff = 120,
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
        --Developer configurations: Not meant for general override
        buffer_previewer_maker = require("telescope.previewers").buffer_previewer_maker,
        mappings = {
          n = {
            ["<C-k>"] = actions.move_selection_previous, -- move to prev result
            ["<C-j>"] = actions.move_selection_next, -- move to next result
            ["<C-d>"] = actions.delete_buffer,
            ["<C-q>"] = actions.send_selected_to_qflist + actions.open_qflist,
            ["q"] = require("telescope.actions").close,
          },
        },
      },

      extensions = {
        ["ui-select"] = {
          require("telescope.themes").get_dropdown({}),
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
    })
    pcall(require("telescope").load_extension, "fzf")
    pcall(require("telescope").load_extension, "file_browser")
    pcall(require("telescope").load_extension, "ui-select")
    --file browser keybinding
    vim.api.nvim_set_keymap("n", "<space>fB", ":Telescope file_browser<CR>", { desc = "file browser", noremap = true })

    -- open file_browser with the path of the current buffer
    vim.api.nvim_set_keymap(
      "n",
      "<space>f",
      ":Telescope file_browser path=%:p:h select_buffer=true<CR>",
      { desc = "file brower in buffer", noremap = true }
    )

    -- open the nvim config file fokder
    vim.api.nvim_set_keymap(
      "n",
      "<space>fc",
      ":Telescope file_browser path=" .. vim.fn.stdpath("config") .. "<CR>",
      { desc = "open the nvim config folder", noremap = true }
    )

    --fzf keybinding

    vim.keymap.set("n", "<leader>sk", builtin.keymaps, { desc = "[S]earch [K]eymaps" })

    vim.keymap.set("n", "<leader>ff", builtin.find_files, { desc = "Find Files" })
    vim.keymap.set("n", "<leader>fg", builtin.git_files, { desc = "Find Git Files" })
    vim.keymap.set("n", "<leader>fh", builtin.help_tags, { desc = "Help for teles" })
    vim.keymap.set("n", "<leader><leader>", builtin.buffers, { desc = "Find buffer" })
    vim.keymap.set("n", "<leader>fs", builtin.lsp_document_symbols, { desc = "Find the LSP" })

    vim.keymap.set("n", "<leader>fi", builtin.live_grep, { desc = "Find live grep" })

    vim.keymap.set("n", "<leader>fs", function()
      builtin.grep_string({ search = vim.fn.input("Grep > ") })
    end, { desc = "Grep search" })
    vim.keymap.set("n", "<leader>fw", function()
      local word = vim.fn.expand("<cword>")
      builtin.grep_string({ search = word })
    end, { desc = "word search" })
    vim.keymap.set("n", "<leader>fW", function()
      local word = vim.fn.expand("<cWORD>")
      builtin.grep_string({ search = word })
    end, { desc = "cWord search" })
  end,
}
