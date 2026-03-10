local wezterm = require("wezterm")
local act = wezterm.action
local is_windows = wezterm.target_triple == "x86_64-pc-windows-msvc"
local is_macos = wezterm.target_triple == "aarch64-apple-darwin"

if is_windows then
  default_prog = { "C:/Program Files/PowerShell/7/pwsh.exe"}
  font_size = 12.0
  front_end = "WebGpu"
  webgpu_power_preference = "HighPerformance"
  animation_fps = 1
  max_fps = 120
  default_font = wezterm.font_with_fallback({
    {
      family = "CaskaydiaCove Nerd Font Mono",
      harfbuzz_features = { "calt=0" },
    },
    {
      family = "Hack Nerd Font",
    },
    {
      family = "JetBrainsMono Nerd Font",
      harfbuzz_features = { "calt=0" },
    }
  })
  window_frame = {
    font = wezterm.font("Consolas", { weight = "Regular" }),
    font_size = 10.0,
  }
  freetype_load_target = "HorizontalLcd"



elseif is_macos then
  default_prog = { "/bin/zsh" , "-l"}
  font_size = 16.0
  front_end = "WebGpu"
  webgpu_power_preference = "HighPerformance"
  animation_fps = 1
  max_fps = 120
  -- default_font = wezterm.font("Hack Nerd Font", { weight = "Regular" })
  default_font = wezterm.font_with_fallback({
    {
      family = "Cascadia Code NF",
      harfbuzz_features = { "calt=0" },
    },
    {
      family = "Hack Nerd Font",
    },
    {
      family = "JetBrainsMono Nerd Font",
      harfbuzz_features = { "calt=0" },
    }
  })
  window_frame = {
    font = wezterm.font("SF Pro Text", { weight = "Regular" }),
    font_size = 14.0,
  }
  freetype_load_target = "Light"

end

-- main config
config = {
  color_scheme = "Dracula",
  allow_win32_input_mode = false,
  enable_kitty_keyboard = true,
  -- enable_kitty_graphics = true,

  default_prog = default_prog,
  font_size = font_size,
  animation_fps = animation_fps,
  font = default_font,
  window_frame = window_frame,
  webgpu_power_preference = webgpu_power_preference,
  window_background_opacity = 0.9,
  freetype_load_target = freetype_load_target,


  -- win32_system_backdrop = "Acrylic"  -- frosted glass effect
  -- win32_acrylic_accent_color = "#10101080"
  max_fps = max_fps,
  scrollback_lines = 3000,

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
    saturation = 0.5,
    brightness = 0.8,
  },
  window_close_confirmation = "AlwaysPrompt",
  enable_tab_bar = true,
  use_fancy_tab_bar = true,
  tab_and_split_indices_are_zero_based = true,

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
      local cwd = pane:get_current_working_directory()
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

config.keys ={
    -- { key = "p", mods = "CTRL|SHIFT", action = act.ActivateCommandPalette },
    { key = "p", mods = "LEADER", action = act.ActivateTabRelative(-1) },
    { key = "n", mods = "LEADER", action = act.ActivateTabRelative(1) },

    -- mode
    { key = "x", mods = "LEADER|CTRL", action = act.SendKey({ key = "x", mods = "CTRL" }) },
  	{ key = "c", mods = "LEADER", action = act.ActivateCopyMode },

    -- Split windows
    { key = "phys:Quote", mods = "CTRL|SHIFT", action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
    { key = "5", mods = "CTRL", action = act.SplitHorizontal({ domain = "DefaultDomain" }) },

    -- Copy/Paste
    { key = "c", mods = "CTRL|SHIFT", action = wezterm.action.CopyTo "Clipboard" },
    { key = "v", mods = "CTRL|SHIFT", action = wezterm.action.PasteFrom "Clipboard" },

    -- Tabs
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

    -- Window
    { key = "n", mods = "CTRL|SHIFT", action = act.SpawnWindow       },

    -- Search
    { key = "f", mods = "CTRL|SHIFT",       action = act.Search { CaseSensitiveString = "" }     },

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
	}
}

-- tab title
wezterm.on('format-tab-title', function(tab)
  local title = tab.tab_title
  if title and #title > 0 then
    -- Use custom title if set
  else
    title = tab.active_pane.title
  end
  if tab.active_pane.is_zoomed then
    title = '[Z] ' .. title
  end
  return title
end)

return config
