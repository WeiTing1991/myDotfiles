local wezterm = require("wezterm")
local act = wezterm.action

local platform = require("platform")
local workspaces = require("workspaces")
local tabbar = require("tabbar")
require("statusbar")

local is_macos = platform.is_macos
local opts = platform.opts

-- Available schemes. These are the exact metadata names from the .toml files --
-- two carry diacritics, and wezterm silently falls back to its defaults if the
-- name doesn't match byte for byte.
-- - Suannhai Jiufen (dark)
-- - Suannhai Lâm-ní (dark)
-- - Suannhai Rouiro (dark)
-- - Suannhai Sumi (dark)
-- - Suannhai Koiai (dark)
-- - Suannhai Hue-pòo (light)
-- - Suannhai Torinoko (light)
-- - Suannhai Shironeri (light)

-- main config
local config = {
  color_scheme_dirs = opts.color_scheme_dirs,
  color_scheme = opts.color_scheme,
  allow_win32_input_mode = true,
  enable_kitty_keyboard = false,

  default_prog = opts.default_prog,
  font_size = opts.font_size,
  font = opts.font,
  window_frame = opts.window_frame,
  front_end = opts.front_end,
  webgpu_power_preference = opts.webgpu_power_preference,
  window_background_opacity = 0.95,
  freetype_load_target = opts.freetype_load_target,

  -- Pane divider weight. WezTerm has no dedicated setting: the split line is
  -- drawn at underline_height, so this also thickens underlines and box glyphs.
  underline_thickness = "3px",

  default_cursor_style = "BlinkingBlock",
  cursor_blink_rate = 500,

  -- win32_system_backdrop = "Acrylic"  -- frosted glass effect
  -- win32_acrylic_accent_color = "#10101080"
  max_fps = opts.max_fps,
  scrollback_lines = 10000,

  -- windows
  adjust_window_size_when_changing_font_size = false,
  window_decorations = "RESIZE",
  window_padding = {
    left = 2,
    right = 2,
    top = 2,
    bottom = 2,
  },
  inactive_pane_hsb = {
    saturation = 0.8,
    brightness = 1,
  },
  window_close_confirmation = "AlwaysPrompt",
  enable_tab_bar = true,
  use_fancy_tab_bar = true,
  tab_and_split_indices_are_zero_based = true,
  -- Default is 16, which cuts names like "data-structure-algorithms-toolkit"
  -- down to nothing once the index and pane count are prepended.
  tab_max_width = 28,

  -- key_bindings
  disable_default_key_bindings = true,
  leader = { key = "x", mods = "CTRL", timeout_milliseconds = 500 },

  hyperlink_rules = wezterm.default_hyperlink_rules(),
}


-- mouse_bindings
config.mouse_bindings = {
  -- Double-click: open cwd in Finder/Explorer
  {
    event = { Up = { streak = 2, button = 'Left' } },
    mods = 'CTRL',
    action = wezterm.action_callback(function(window, pane)
      local cwd = pane:get_current_working_dir()
      if cwd then
        if is_macos then
          wezterm.open_with(cwd.file_path, 'Finder')
        else
          wezterm.open_with(cwd.file_path, 'explorer')
        end
      end
    end),
  },
  {
    event = { Up = { streak = 2, button = 'Right' } },
    mods = 'NONE',
    action = act.CopyTo('Clipboard'),
  },
  {
    event = { Up = { streak = 1, button = 'Left' } },
    mods = is_macos and 'SUPER' or 'CTRL',
    action = act.OpenLinkAtMouseCursor,
  },
}

local function tab_title(tab)
  local user_title = tab:get_title()
  if user_title and #user_title > 0 then return user_title end
  local active_pane
  for _, info in ipairs(tab:panes_with_info()) do
    if info.is_active then active_pane = info.pane; break end
  end
  if not active_pane then return "?" end
  local ok, cwd = pcall(function() return active_pane:get_current_working_dir() end)
  if ok and cwd then
    return cwd.file_path:match("([^/\\]+)/?$") or cwd.file_path
  end
  local ok2, t = pcall(function() return active_pane:get_title() end)
  return (ok2 and t) or "?"
end

config.keys ={
    -- { key = "p", mods = "CTRL|SHIFT", action = act.ActivateCommandPalette },
    { key = "k", mods = "LEADER", action = act.ActivateTabRelative(-1) },
    { key = "j", mods = "LEADER", action = act.ActivateTabRelative(1) },
    { key = "phys:Comma",  mods = "LEADER|SHIFT", action = act.MoveTabRelative(-1) },
    { key = "phys:Period", mods = "LEADER|SHIFT", action = act.MoveTabRelative(1) },
    { key = "<", mods = "LEADER", action = act.MoveTabRelative(-1) },
    { key = ">", mods = "LEADER", action = act.MoveTabRelative(1) },

    -- mode
    { key = "x", mods = "LEADER|CTRL", action = act.SendKey({ key = "x", mods = "CTRL" }) },
  	{ key = "c", mods = "LEADER", action = act.ActivateCopyMode },

    -- Split windows
    { key = "phys:Quote", mods = "CTRL|SHIFT", action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
    { key = "5", mods = "CTRL", action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },

    -- Copy/Paste
    { key = "c", mods = "CTRL|SHIFT", action = wezterm.action.CopyTo "Clipboard" },
    { key = "v", mods = "CTRL|SHIFT", action = wezterm.action.PasteFrom "Clipboard" },

    -- Tabs
    { key = "t", mods = "LEADER", action = wezterm.action_callback(function(window, pane)
      local choices = {}
      local build_ok, build_err = pcall(function()
        for i, t in ipairs(window:mux_window():tabs()) do
          local ok, title = pcall(tab_title, t)
          table.insert(choices, {
            label = string.format("%d: %s", i, ok and title or ("tab " .. i)),
            id    = tostring(i - 1),
          })
        end
      end)
      if not build_ok then
        wezterm.log_error("tab-nav build error: " .. tostring(build_err))
        choices = {{ label = "error - check debug overlay", id = "0" }}
      end
      window:perform_action(
        act.InputSelector {
          title   = "Go to Tab",
          choices = choices,
          fuzzy   = true,
          action  = wezterm.action_callback(function(w, p, id, _)
            if not id then return end
            w:perform_action(act.ActivateTab(tonumber(id)), p)
          end),
        },
        pane
      )
    end) },
    { key = "t", mods = "CTRL|SHIFT", action = wezterm.action.SpawnTab "CurrentPaneDomain" },
    { key = "w", mods = "CTRL|SHIFT", action = wezterm.action.CloseCurrentTab { confirm = false } },

    -- Font size
    { key = "=", mods = "CTRL", action = wezterm.action.IncreaseFontSize },
    { key = "-", mods = "CTRL", action = wezterm.action.DecreaseFontSize },
    { key = "0", mods = "CTRL", action = act.ResetFontSize},

    -- Pane navigation
    { key = "h", mods = "CTRL|SHIFT", action = act.ActivatePaneDirection("Left") },
    { key = "j", mods = "CTRL|SHIFT", action = act.ActivatePaneDirection("Down") },
    { key = "k", mods = "CTRL|SHIFT", action = act.ActivatePaneDirection("Up") },
    { key = "l", mods = "CTRL|SHIFT", action = act.ActivatePaneDirection("Right") },
    { key = "m", mods = "CTRL|SHIFT", action = act.TogglePaneZoomState },

    -- Pane list: overlays a label on each pane, press it to jump there
    { key = "p", mods = "LEADER", action = act.PaneSelect { show_pane_ids = true } },
    { key = "P", mods = "LEADER", action = act.PaneSelect { mode = "SwapWithActive" } },

    -- Pane resize: hold the mode and repeat hjkl, exits after 1s idle
    { key = "r", mods = "LEADER", action = act.ActivateKeyTable {
      name = "resize_pane",
      one_shot = false,
      timeout_milliseconds = 1000,
    }},

    -- Window
    { key = "n", mods = "CTRL|SHIFT", action = act.SpawnWindow       },

    -- Search (prefills from the current selection, like an IDE find bar)
    { key = "f", mods = "CTRL|SHIFT", action = act.Search("CurrentSelectionOrEmptyString") },
    { key = "f", mods = "SUPER",      action = act.Search("CurrentSelectionOrEmptyString") },

    -- Alt keys (readline)
    -- { key = "f",         mods = "ALT", action = wezterm.action.SendString("\x1bf")    },
    -- { key = "b",         mods = "ALT", action = wezterm.action.SendString("\x1bb")    },
    -- { key = "d",         mods = "ALT", action = wezterm.action.SendString("\x1bd")    },
    -- { key = "Backspace", mods = "ALT", action = wezterm.action.SendString("\x1b\x7f") },

    { key = "r", mods = "SHIFT|CTRL", action = act.PromptInputLine {
      description = "Enter new name for tab",
      action = wezterm.action_callback(function(window, pane, line)
        if line then
          window:active_tab():set_title(line)
        end
      end),
    }},
}

-- Workspace pickers (Ctrl+X w / Ctrl+X W)
for _, key in ipairs(workspaces.keys) do
  table.insert(config.keys, key)
end

-- Jump straight to a tab by the index shown in its title. Zero-based, to match
-- tab_and_split_indices_are_zero_based.
for i = 0, 9 do
  table.insert(config.keys, {
    key = tostring(i),
    mods = "LEADER",
    action = act.ActivateTab(i),
  })
end

-- Session persistence. plugin.require clones from GitHub on first run; if that
-- machine is offline the whole config would otherwise fail to load, leaving no
-- usable terminal. Degrade to the pickers above instead.
local ok, resurrect = pcall(wezterm.plugin.require, "https://github.com/MLFlexer/resurrect.wezterm")
if ok then
  resurrect.state_manager.periodic_save()
  wezterm.on("gui-startup", resurrect.state_manager.resurrect_on_gui_startup)

  table.insert(config.keys, {
    key = "s", mods = "LEADER",
    action = wezterm.action_callback(function()
      resurrect.state_manager.save_state(resurrect.workspace_state.get_workspace_state())
    end),
  })
  table.insert(config.keys, {
    key = "l", mods = "LEADER",
    action = wezterm.action_callback(function(window, pane)
      resurrect.fuzzy_loader.fuzzy_load(window, pane, function(id)
        local state = resurrect.state_manager.load_state(id:match("([^/]+)%.json$"), "workspace")
        resurrect.workspace_state.restore_workspace(state, { relative = true, restore_text = true })
      end)
    end),
  })
else
  wezterm.log_warn("resurrect.wezterm unavailable: " .. tostring(resurrect))
end

config.key_tables = {
	copy_mode = {
    { key = 'h', mods = 'NONE', action = act.CopyMode('MoveLeft') },
    { key = 'j', mods = 'NONE', action = act.CopyMode('MoveDown') },
    { key = 'k', mods = 'NONE', action = act.CopyMode('MoveUp') },
    { key = 'l', mods = 'NONE', action = act.CopyMode('MoveRight') },

    { key = "Escape", mods = 'NONE', action = act.CopyMode 'Close' },
    { key = 'v', mods = 'NONE', action = act.CopyMode{ SetSelectionMode =  'Cell' } },
    { key = 'v', mods = 'CTRL', action = act.CopyMode{ SetSelectionMode =  'Block' } },

  	{ key = "c", mods = "SHIFT|CTRL", action = act.CopyTo("Clipboard") },
  	{ key = "c", mods = "SHIFT|SUPER", action = act.CopyTo("Clipboard") },
  	{ key = "v", mods = "SHIFT|CTRL", action = act.PasteFrom("Clipboard") },
  	{ key = "v", mods = "SHIFT|SUPER", action = act.PasteFrom("Clipboard") },

    {
      key = 'y',
      mods = 'NONE',
      action = act.Multiple {
        { CopyTo = 'ClipboardAndPrimarySelection' },
        { CopyMode = 'Close' },
      },
    },

    -- Search navigation
    { key = 'n', mods = 'NONE',  action = act.CopyMode('NextMatch') },
    { key = 'n', mods = 'SHIFT', action = act.CopyMode('PriorMatch') },
    { key = '/', mods = 'NONE',  action = act.CopyMode('EditPattern') },
    { key = 'd', mods = 'CTRL',  action = act.CopyMode('PageDown') },
    { key = 'u', mods = 'CTRL',  action = act.CopyMode('PageUp') },
    { key = 'g', mods = 'NONE',  action = act.CopyMode('MoveToScrollbackTop') },
    { key = 'g', mods = 'SHIFT', action = act.CopyMode('MoveToScrollbackBottom') },
	},

	resize_pane = {
    { key = 'h', mods = 'NONE',  action = act.AdjustPaneSize({ 'Left', 3 }) },
    { key = 'j', mods = 'NONE',  action = act.AdjustPaneSize({ 'Down', 3 }) },
    { key = 'k', mods = 'NONE',  action = act.AdjustPaneSize({ 'Up', 3 }) },
    { key = 'l', mods = 'NONE',  action = act.AdjustPaneSize({ 'Right', 3 }) },

    -- SHIFT for fine adjustment
    { key = 'h', mods = 'SHIFT', action = act.AdjustPaneSize({ 'Left', 1 }) },
    { key = 'j', mods = 'SHIFT', action = act.AdjustPaneSize({ 'Down', 1 }) },
    { key = 'k', mods = 'SHIFT', action = act.AdjustPaneSize({ 'Up', 1 }) },
    { key = 'l', mods = 'SHIFT', action = act.AdjustPaneSize({ 'Right', 1 }) },

    { key = 'Escape', mods = 'NONE', action = 'PopKeyTable' },
    { key = 'Enter',  mods = 'NONE', action = 'PopKeyTable' },
	},

	-- Find bar: Enter goes forward (wezterm's default sends it backwards)
	search_mode = {
    { key = 'Enter',     mods = 'NONE',  action = act.CopyMode('NextMatch') },
    { key = 'Enter',     mods = 'SHIFT', action = act.CopyMode('PriorMatch') },
    { key = 'DownArrow', mods = 'NONE',  action = act.CopyMode('NextMatch') },
    { key = 'UpArrow',   mods = 'NONE',  action = act.CopyMode('PriorMatch') },
    { key = 'n',         mods = 'CTRL',  action = act.CopyMode('NextMatch') },
    { key = 'p',         mods = 'CTRL',  action = act.CopyMode('PriorMatch') },
    { key = 'PageDown',  mods = 'NONE',  action = act.CopyMode('NextMatchPage') },
    { key = 'PageUp',    mods = 'NONE',  action = act.CopyMode('PriorMatchPage') },
    { key = 'r',         mods = 'CTRL',  action = act.CopyMode('CycleMatchType') },
    { key = 'u',         mods = 'CTRL',  action = act.CopyMode('ClearPattern') },
    { key = 'Escape',    mods = 'NONE',  action = act.CopyMode('Close') },

    { key = 'c', mods = 'SHIFT|CTRL', action = act.CopyTo('Clipboard') },
    { key = 'c', mods = 'SUPER',      action = act.CopyTo('Clipboard') },
	}
}

-- Tab titles and frame colors live in tabbar.lua. apply_to_config must run
-- after color_scheme/window_frame are set: it reads them.
tabbar.apply_to_config(config)

local dispatcher = require("dispatcher")
dispatcher.apply_to_config(config, {
  nvim_bin = opts.nvim_bin,
  focus_nvim = true,
  debug = true,      -- set false once it works
})

return config

