local neotree = require("neo-tree")

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

neotree.setup({
  close_if_last_window = false,
  popup_border_style = "rounded",
  enable_git_status = true,
  enable_diagnostics = false,
  sort_case_insensitive = true,
  sources = {
    "filesystem",
    "git_status",
    "buffers",
    "document_symbols",
  },
  source_selector = {
    winbar = true,
    statusline = false,
    show_scrolled_off_parent_node = false,
    tabs_layout = "equal",
    content_layout = "start",
    sources = {
      { source = "filesystem", display_name = "󰉓  Files" },
      { source = "git_status", display_name = "󰊢  Git" },
      -- { source = "buffers", display_name = "󰈙 Buffers" },
      -- { source = "document_symbols", display_name = "󰌗 Symbols" },
    },
  },

  default_component_configs = {
    indent = {
      indent_size = 2,
      padding = 1,
      with_markers = true,
      indent_marker = "│",
      last_indent_marker = "└",
      highlight = "NeoTreeIndentMarker",
    },
    -- icon = {
    --   folder_closed = icon.tree.glyphs.folder.default or "",
    --   folder_open = icon.tree.glyphs.folder.open or "",
    --   folder_empty = icon.tree.glyphs.folder.empty or "",
    --   default = icon.tree.glyphs.default or "*",
    -- },
    git_status = {
      align = "right",
      -- symbols = {
      --   added     = icon.tree.glyphs.git.added or "",
      --   modified  = icon.tree.glyphs.git.modified or "",
      --   deleted   = icon.tree.glyphs.git.deleted or "✖",
      --   renamed   = icon.tree.glyphs.git.renamed or "󰁕",
      --   untracked = icon.tree.glyphs.git.untracked or "",
      --   ignored   = icon.tree.glyphs.git.ignored or "",
      --   unstaged  = icon.tree.glyphs.git.unstaged or "󰄱",
      --   staged    = icon.tree.glyphs.git.staged or "",
      --   conflict  = icon.tree.glyphs.git.conflict or "",
      -- }
    },
  },

  window = {
    position = "right",
    width = 40,
    mapping_options = {
      noremap = true,
      nowait = true,
    },
    mappings = {
      -- ["<C-e>"] = "close_window",
      -- ["z"] = "close_all_nodes",
      ["n"] = {
        "add",
        config = {
          show_path = "none",
        },
      },
      -- ["<cr>"] = "open",
      -- ["<esc>"] = "cancel",
      -- ["P"] = {
      --   "toggle_preview",
      --   config = {
      --     use_float = true,
      --     use_image_nvim = true
      --   }
      -- },
      -- ["l"] = "focus_preview",
      -- ["S"] = "open_split",
      -- ["s"] = "open_vsplit",
      -- ["t"] = "open_tabnew",
      -- ["C"] = "close_node",
      -- ["a"] = {
      --   "add",
      --   config = {
      --     show_path = "none"
      --   }
      -- },
      -- ["A"] = "add_directory",
      -- ["d"] = "delete",
      -- ["r"] = "rename",
      -- ["y"] = "copy_to_clipboard",
      -- ["x"] = "cut_to_clipboard",
      -- ["p"] = "paste_from_clipboard",
      -- ["c"] = "copy",
      -- ["m"] = "move",
      -- ["q"] = "close_window",
      -- ["R"] = "refresh",
      -- ["?"] = "show_help",
      -- ["<"] = "prev_source",
      -- [">"] = "next_source",
    },
  },

  -- files
  filesystem = {
    filtered_items = {
      visible = true, -- Show filtered items with different highlight
      hide_dotfiles = false,
      hide_gitignored = false,
      hide_by_name = {
        ".DS_Store",
      },
      never_show = {
        ".git",
      },
    },
    follow_current_file = {
      enabled = true,
      leave_dirs_open = false,
    },
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
      local is_windows = vim.loop.os_uname().sysname == "Windows_NT"
      local is_mac = vim.loop.os_uname().sysname == "Darwin"

      if is_mac then
        vim.fn.jobstart({ "open", "-R", path }, { detach = true })
      elseif is_windows then
        local p
        local lastSlashIndex = path:match("^.+()\\[^\\]*$")
        if lastSlashIndex then
          p = path:sub(1, lastSlashIndex - 1)
        else
          p = path
        end
        vim.cmd("silent !start explorer " .. p)
      else
        vim.fn.jobstart({ "xdg-open", path }, { detach = true })
      end
    end,
  },
  -- git
  git_status = {
    group_empty_dirs = false,
    renderers = {
      file = {
        { "indent", indent_size = 1, padding = 0, with_markers = false },
        { "icon" },
        { "name", use_git_status_colors = true },
        { "git_status", highlight = "NeoTreeDimText", align = "right" },
      },
    },
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

-- Highlight for git ignored files
vim.api.nvim_set_hl(0, "NeoTreeGitIgnored", {
  fg = "#6c7086",

  italic = true,
})
