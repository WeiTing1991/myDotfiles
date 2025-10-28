local fzf = require("fzf-lua")
local actions = fzf.actions

-- https://github.com/MariaSolOs/dotfiles/blob/main/.config/nvim/lua/plugins/fzf-lua.lua
fzf.setup({
  winopts = {
    -- walk around because the
    height = 0.95, -- Almost fullscreen as default
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
  -- defaults = { git_icons = false },
  -- lsp = {
  --   preview = true,
  -- },
  -- oldfiles = {
  --   include_current_session = true,
  --   winopts = {
  --     preview = { hidden = true },
  --   },
  -- },
  keymap = {
    builtin = {
      -- true,        -- uncomment to inherit all the below in your custom config
      ["<M-Esc>"] = "hide", -- hide fzf-lua, `:FzfLua resume` to continue
      ["<F1>"] = "toggle-help",
      ["<F2>"] = "toggle-fullscreen",
      -- Only valid with the 'builtin' previewer
      ["<F3>"] = "toggle-preview-wrap",
      ["<F4>"] = "toggle-preview",
      -- Rotate preview clockwise/counter-clockwise
      ["<F5>"] = "toggle-preview-ccw",
      ["<F6>"] = "toggle-preview-cw",
      -- `ts-ctx` binds require `nvim-treesitter-context`
      ["<F7>"] = "toggle-preview-ts-ctx",
      ["<F8>"] = "preview-ts-ctx-dec",
      ["<F9>"] = "preview-ts-ctx-inc",
      ["<S-Left>"] = "preview-reset",
      ["<S-down>"] = "preview-page-down",
      ["<S-up>"] = "preview-page-up",
      ["<M-S-down>"] = "preview-down",
      ["<M-S-up>"] = "preview-up",
    },
    fzf = {
      -- fzf '--bind=' options
      -- ["ctrl-z"] = "abort",
      -- ["ctrl-u"] = "unix-line-discard",
      -- ["ctrl-f"] = "half-page-down",
      -- ["ctrl-b"] = "half-page-up",
      -- ["ctrl-a"] = "beginning-of-line",
      -- ["ctrl-e"] = "end-of-line",
      ["ctrl-a"] = "toggle-all",
      ["ctrl-g"] = "first",
      ["ctrl-G"] = "last",
      ["ctrl-q"] = "select-all+accept",
      -- Only valid with fzf previewers (bat/cat/git/etc)
      ["f3"] = "toggle-preview-wrap",
      ["f4"] = "toggle-preview",
      ["shift-down"] = "preview-page-down",
      ["shift-up"] = "preview-page-up",
    },
  }, -- Neovim keymaps / fzf binds
  actions = {
    -- Below are the default actions, setting any value in these tables will override
    -- the defaults, to inherit from the defaults change [1] from `false` to `true`
    files = {
      -- true,        -- uncomment to inherit all the below in your custom config
      -- Pickers inheriting these actions:
      --   files, git_files, git_status, grep, lsp, oldfiles, quickfix, loclist,
      --   tags, btags, args, buffers, tabs, lines, blines
      -- `file_edit_or_qf` opens a single selection or sends multiple selection to quickfix
      -- replace `enter` with `file_edit` to open all files/bufs whether single or multiple
      -- replace `enter` with `file_switch_or_edit` to attempt a switch in current tab first
      ["enter"] = FzfLua.actions.file_edit_or_qf,
      ["ctrl-s"] = FzfLua.actions.file_split,
      ["ctrl-v"] = FzfLua.actions.file_vsplit,
      ["ctrl-t"] = FzfLua.actions.file_tabedit,
      ["alt-q"] = FzfLua.actions.file_sel_to_qf,
      ["alt-Q"] = FzfLua.actions.file_sel_to_ll,
      ["alt-i"] = FzfLua.actions.toggle_ignore,
      ["alt-h"] = FzfLua.actions.toggle_hidden,
      ["alt-f"] = FzfLua.actions.toggle_follow,
    },
  }, -- Fzf "accept" binds
  fzf_opts = {
    ["--no-scrollbar"] = true,
  }, -- Fzf CLI flags
  fzf_colors = {
    true,
  },
  -- Fzf `--color` specification
  -- hls = { ...  },         -- Highlights
  -- previewers = { ...  },  -- Previewers options

  -- SPECIFIC COMMAND/PICKER OPTIONS, SEE BELOW
  files = {
    cwd_prompt = true,
    fzf_opts = {
      ["--exact"] = "",
      ["--no-sort"] = "",
    },
    actions = {
      ["alt-i"] = { actions.toggle_ignore },
      ["alt-h"] = { actions.toggle_hidden },
    },
  },
  grep = {
    -- header_prefix = icons.misc.search .. " ",
    rg_glob_fn = function(query, opts)
      local regex, flags = query:match(string.format("^(.*)%s(.*)$", opts.glob_separator))
      -- Return the original query if there's no separator.
      return (regex or query), flags
    end,
    actions = {
      ["alt-i"] = { actions.toggle_ignore },
      ["alt-h"] = { actions.toggle_hidden },
    },
  },
  previewers = {
    builtin = {
      snacks_image = {
        enabled = false,
      },
      extensions = {
        ["png"] = { "chafa" },
        ["jpg"] = { "chafa" },
        ["jpeg"] = { "chafa" },
        ["gif"] = { "chafa" },
      },
    },
  },
})

require("fzf-lua").register_ui_select(function(_, items)
  return {
    winopts = {
      height = 0.4,
      width = 0.6,
      row = 0.5,
      col = 0.5,
    },
  }
end)
