local fzf = require("fzf-lua")
local config = fzf.config
local actions = fzf.actions

-- NOTE: https://github.com/ibhagwan/fzf-lua?tab=readme-ov-file
fzf.setup({
  -- MISC GLOBAL SETUP OPTIONS, SEE BELOW
  -- fzf_bin = ...,
  -- UI options
  winopts = {
      width = 0.9,
      height = 0.9,
      row = 0.5,
      col = 0.5,
      preview = {
        scrollchars = { "┃", "" },
      },
    },
  keymap = {
    -- Below are the default binds, setting any value in these tables will override
    -- the defaults, to inherit from the defaults change [1] from `false` to `true`
    builtin = {
      -- neovim `:tmap` mappings for the fzf win
      -- true,        -- uncomment to inherit all the below in your custom config
      ["<M-Esc>"]     = "hide",     -- hide fzf-lua, `:FzfLua resume` to continue
      ["<F1>"]        = "toggle-help",
      ["<F2>"]        = "toggle-fullscreen",
      -- Only valid with the 'builtin' previewer
      ["<F3>"]        = "toggle-preview-wrap",
      ["<F4>"]        = "toggle-preview",
      -- Rotate preview clockwise/counter-clockwise
      ["<F5>"]        = "toggle-preview-ccw",
      ["<F6>"]        = "toggle-preview-cw",
      -- `ts-ctx` binds require `nvim-treesitter-context`
      ["<F7>"]        = "toggle-preview-ts-ctx",
      ["<F8>"]        = "preview-ts-ctx-dec",
      ["<F9>"]        = "preview-ts-ctx-inc",
      ["<S-Left>"]    = "preview-reset",
      ["<S-down>"]    = "preview-page-down",
      ["<S-up>"]      = "preview-page-up",
      ["<M-S-down>"]  = "preview-down",
      ["<M-S-up>"]    = "preview-up",
    },
    fzf = {
      -- fzf '--bind=' options
      -- true,        -- uncomment to inherit all the below in your custom config
      ["ctrl-z"]      = "abort",
      ["ctrl-u"]      = "unix-line-discard",
      ["ctrl-f"]      = "half-page-down",
      ["ctrl-b"]      = "half-page-up",
      ["ctrl-a"]      = "beginning-of-line",
      ["ctrl-e"]      = "end-of-line",
      ["alt-a"]       = "toggle-all",
      ["alt-g"]       = "first",
      ["alt-G"]       = "last",
      -- Only valid with fzf previewers (bat/cat/git/etc)
      ["f3"]          = "toggle-preview-wrap",
      ["f4"]          = "toggle-preview",
      ["shift-down"]  = "preview-page-down",
      ["shift-up"]    = "preview-page-up",
    },
  },      -- Neovim keymaps / fzf binds
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
      ["enter"]       = actions.file_edit_or_qf,
      ["ctrl-s"]      = actions.file_split,
      ["ctrl-v"]      = actions.file_vsplit,
      ["ctrl-t"]      = actions.file_tabedit,
      ["alt-q"]       = actions.file_sel_to_qf,
      ["alt-Q"]       = actions.file_sel_to_ll,
      ["alt-i"]       = actions.toggle_ignore,
      ["alt-h"]       = actions.toggle_hidden,
      ["alt-f"]       = actions.toggle_follow,
    },
  },     -- Fzf "accept" binds
  fzf_opts = {
    ["--no-scrollbar"] = true,
  }, -- Fzf CLI flags
  fzf_colors = {
    true,
  }, -- Fzf `--color` specification
  -- hls = { ...  },         -- Highlights
  -- previewers = { ...  },  -- Previewers options

  -- SPECIFIC COMMAND/PICKER OPTIONS, SEE BELOW
  files = {
    cwd_prompt = true,
    actions = {
      ["alt-i"] = { actions.toggle_ignore },
      ["alt-h"] = { actions.toggle_hidden },
    },
  },
  grep = {
    actions = {
      ["alt-i"] = { actions.toggle_ignore },
      ["alt-h"] = { actions.toggle_hidden },
    },
  },
})
