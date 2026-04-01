return {
  -- Fuzzy finder
  {
    "ibhagwan/fzf-lua",
    cmd = "FzfLua",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    keys = {
      { "<C-p>", function() require("fzf-lua").files() end, desc = "Find files" },
      { "<leader>ff", function() require("fzf-lua").files() end, desc = "Find files" },
      { "<leader>fl", function() require("fzf-lua").live_grep() end, desc = "Live grep" },
      { "<leader>fg", function() require("fzf-lua").grep_curbuf() end, desc = "Grep current buffer" },
      { "<leader>fb", function() require("fzf-lua").buffers() end, desc = "Find buffer" },
      { "<leader><leader>", function() require("fzf-lua").buffers() end, desc = "Find buffer" },
      { "z=", function() require("fzf-lua").spell_suggest() end, desc = "Spell suggest" },
    },
    config = function()
      local fzf = require("fzf-lua")
      local actions = fzf.actions

      fzf.setup({
        winopts = {
          height = 0.95,
          width = 0.95,
          row = 0.5,
          col = 0.5,
          preview = {
            scrollbar = false,
            layout = "vertical",
            vertical = "up:70%",
            delay = 150,
          },
        },
        keymap = {
          builtin = {
            ["<M-Esc>"] = "hide",
            ["<F1>"] = "toggle-help",
            ["<F2>"] = "toggle-fullscreen",
            ["<F3>"] = "toggle-preview-wrap",
            ["<F4>"] = "toggle-preview",
            ["<S-down>"] = "preview-page-down",
            ["<S-up>"] = "preview-page-up",
          },
          fzf = {
            ["alt-j"] = "down",
            ["alt-k"] = "up",
            ["ctrl-a"] = "toggle-all",
            ["ctrl-g"] = "first",
            ["ctrl-G"] = "last",
            ["ctrl-q"] = "select-all+accept",
            ["f3"] = "toggle-preview-wrap",
            ["f4"] = "toggle-preview",
            ["shift-down"] = "preview-page-down",
            ["shift-up"] = "preview-page-up",
          },
        },
        actions = {
          files = {
            ["enter"] = actions.file_edit_or_qf,
            ["ctrl-s"] = actions.file_split,
            ["ctrl-v"] = actions.file_vsplit,
            ["ctrl-t"] = require("trouble.sources.fzf").open,
            ["alt-q"] = actions.file_sel_to_qf,
            ["alt-Q"] = actions.file_sel_to_ll,
            ["alt-i"] = actions.toggle_ignore,
            ["alt-h"] = actions.toggle_hidden,
            ["alt-f"] = actions.toggle_follow,
          },
        },
        fzf_opts = { ["--no-scrollbar"] = true },
        fzf_colors = { true },
        files = {
          cwd_prompt = true,
          fzf_opts = { ["--exact"] = "", ["--no-sort"] = "" },
          actions = {
            ["alt-i"] = { actions.toggle_ignore },
            ["alt-h"] = { actions.toggle_hidden },
          },
        },
        grep = {
          rg_glob_fn = function(query, opts)
            local regex, flags = query:match(string.format("^(.*)%s(.*)$", opts.glob_separator))
            return (regex or query), flags
          end,
          actions = {
            ["alt-i"] = { actions.toggle_ignore },
            ["alt-h"] = { actions.toggle_hidden },
          },
        },
        buffers = {
          winopts = { height = 0.60, width = 0.60 },
          previewer = false,
        },
        previewers = {
          builtin = {
            snacks_image = { enabled = false },
            extensions = {
              ["png"] = { "chafa" },
              ["jpg"] = { "chafa" },
              ["jpeg"] = { "chafa" },
              ["gif"] = { "chafa" },
            },
          },
        },
      })

      fzf.register_ui_select(function()
        return {
          winopts = { height = 0.4, width = 0.6, row = 0.5, col = 0.5 },
        }
      end)
    end,
  },

  -- File explorer (dired-style)
  {
    "stevearc/oil.nvim",
    cmd = "Oil",
    keys = {
      { "<leader>d", function() require("oil").open() end, desc = "Open file explorer" },
    },
    opts = {
      default_file_explorer = false,
      delete_to_trash = true,
      skip_confirm_for_simple_edits = true,
      win_options = {
        wrap = false,
        signcolumn = "yes",
        cursorcolumn = false,
        foldcolumn = "1",
        spell = false,
        list = true,
        conceallevel = 1,
        concealcursor = "nvic",
      },
      view_options = {
        show_hidden = true,
        natural_order = true,
      },
      float = {
        padding = 2,
        max_width = 0,
        max_height = 0,
        border = "rounded",
        win_options = { winblend = 0 },
        preview_split = "below",
      },
      preview_win = {
        update_on_cursor_moved = true,
        preview_method = "fast_scratch",
      },
      keymaps = {
        ["g?"] = { "actions.show_help", mode = "n" },
        ["<C-p>"] = "actions.preview",
        ["q"] = "actions.close",
        ["<C-c>"] = "actions.close",
      },
    },
  },

  -- File tree (sidebar)
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    cmd = "Neotree",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "MunifTanjim/nui.nvim",
      "nvim-tree/nvim-web-devicons",
      "antosha417/nvim-lsp-file-operations",
    },
    keys = {
      { "<C-e>", function() require("neo-tree.command").execute({ toggle = true, dir = vim.uv.cwd() }) end, desc = "File tree" },
    },
    config = function()
      vim.g.loaded_netrw = 1
      vim.g.loaded_netrwPlugin = 1

      require("neo-tree").setup({
        close_if_last_window = false,
        popup_border_style = "rounded",
        enable_git_status = true,
        enable_diagnostics = false,
        sort_case_insensitive = true,
        sources = { "filesystem", "git_status", "buffers", "document_symbols" },
        source_selector = {
          winbar = true,
          statusline = false,
          tabs_layout = "equal",
          content_layout = "start",
          sources = {
            { source = "filesystem", display_name = "  Files" },
            { source = "git_status", display_name = "  Git" },
          },
        },
        default_component_configs = {
          indent = {
            indent_size = 2,
            padding = 1,
            with_markers = true,
            indent_marker = "|",
            last_indent_marker = "|",
            highlight = "NeoTreeIndentMarker",
          },
          git_status = { align = "right" },
        },
        window = {
          position = "right",
          width = 40,
          mapping_options = { noremap = true, nowait = true },
          mappings = {
            ["n"] = { "add", config = { show_path = "none" } },
          },
        },
        filesystem = {
          filtered_items = {
            visible = true,
            hide_dotfiles = false,
            hide_gitignored = false,
            hide_by_name = { ".DS_Store" },
            never_show = { ".git" },
          },
          follow_current_file = { enabled = true, leave_dirs_open = false },
          group_empty_dirs = false,
          hijack_netrw_behavior = "open_default",
          use_libuv_file_watcher = true,
          window = {
            mappings = {
              ["<bs>"] = "navigate_up",
              ["."] = "set_root",
              ["H"] = "toggle_hidden",
              ["/"] = "fuzzy_finder",
              ["D"] = "fuzzy_finder_directory",
              ["f"] = "filter_on_submit",
              ["<c-x>"] = "clear_filter",
              ["[g"] = "prev_git_modified",
              ["]g"] = "next_git_modified",
              ["O"] = "system_open",
            },
          },
        },
        commands = {
          system_open = function(state)
            local node = state.tree:get_node()
            local path = node:get_id()
            local utils = require("core.utils")
            if utils.is_mac then
              vim.fn.jobstart({ "open", "-R", path }, { detach = true })
            elseif utils.is_windows then
              local p = path:match("^.+()\\[^\\]*$") and path:sub(1, path:match("^.+()\\[^\\]*$") - 1) or path
              vim.cmd("silent !start explorer " .. p)
            else
              vim.fn.jobstart({ "xdg-open", path }, { detach = true })
            end
          end,
        },
        git_status = {
          group_empty_dirs = false,
          window = {
            position = "right",
            mappings = {
              ["A"] = "git_add_all",
              ["gu"] = "git_unstage_file",
              ["ga"] = "git_add_file",
              ["gr"] = "git_revert_file",
              ["gc"] = "git_commit",
              ["gp"] = "git_push",
              ["gg"] = "git_commit_and_push",
            },
          },
        },
      })

      require("lsp-file-operations").setup()

      vim.api.nvim_set_hl(0, "NeoTreeGitIgnored", { fg = "#6c7086", italic = true })
    end,
  },
}
