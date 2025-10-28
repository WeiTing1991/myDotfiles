-- Pull in the wezterm API
local wezterm = require("wezterm")
local act = wezterm.action

-- This will hold the configuration.
local config = wezterm.config_builder()

-- Detect the operating system
local is_windows = wezterm.target_triple == "x86_64-pc-windows-msvc"
-- aarch special for m-series mac
local is_macos = wezterm.target_triple == "aarch64-apple-darwin"

local default_prog
local font_size
local default_font
local launch_menu = {}

if is_windows then
  default_prog = { "pwsh.exe" }
  font_size = 12.0
  -- default_font = wezterm.font("Consolas")
  -- default_font = wezterm.font("ZenMono Nerd Font")
  -- default_font = wezterm.font("Hack Nerd Font", { weight = "Regular" })
  default_font = wezterm.font_with_fallback({
    {
      family = "Hack Nerd Font",
      harfbuzz_features = { "calt=0" },
    },
  })
elseif is_macos then
  default_prog = { "/bin/zsh" }
  font_size = 13.0
  default_font = wezterm.font_with_fallback({
    {
      family = "Hack Nerd Font",
      harfbuzz_features = { "calt=0" },
    },
  })
  -- default_font = wezterm.font("Hack Nerd Font", { weight = "Regular" })
end

config = {
  -- enable ctrl key
  allow_win32_input_mode = false,
  enable_kitty_keyboard = true,
  enable_kitty_graphics = true,
  hyperlink_rules = wezterm.default_hyperlink_rules(),

  -- render option
  -- enable_wayland = false,
  front_end = "WebGpu",
  -- high performance rendering has issue
  -- webgpu_power_preference = "HighPerformance",
  -- webgpu_preferred_adapter  = wezterm.gui.enumerate_gpus()[1],

  max_fps = 166,
  animation_fps = 60,
  freetype_load_target = "Normal",

  default_prog = default_prog,
  font_size = font_size,
  -- color_scheme = "GruvboxDarkHard",
  color_scheme = "Dracula",
  -- color_scheme = "rose-pine",
  initial_cols = 120,
  initial_rows = 50,

  font = default_font,
  -- windows
  window_background_opacity = 1,
  window_decorations = "RESIZE",
  window_padding = {
    left = 5,
    right = 5,
    top = 10,
    bottom = 10,
  },
  inactive_pane_hsb = {
    saturation = 0.5,
    brightness = 0.8,
  },
  window_close_confirmation = "AlwaysPrompt",
  adjust_window_size_when_changing_font_size = false,
  enable_tab_bar = true,
  use_fancy_tab_bar = true,
  tab_and_split_indices_are_zero_based = true,
  scrollback_lines = 3000,

  -- key_bindings
  disable_default_key_bindings = true,
  leader = { key = "x", mods = "CTRL", timeout_milliseconds = 500 },
  keys = {
    -- mode
    { key = "x", mods = "LEADER|CTRL", action = act.SendKey({ key = "x", mods = "CTRL" }) },

    -- Split windows
    { key = "phys:Quote", mods = "CTRL|SHIFT", action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
    { key = "5", mods = "CTRL", action = act.SplitHorizontal({ domain = "DefaultDomain" }) },

    -- Pane navigation
    { key = "h", mods = "CTRL|SHIFT", action = act.ActivatePaneDirection("Left") },
    { key = "j", mods = "CTRL|SHIFT", action = act.ActivatePaneDirection("Down") },
    { key = "k", mods = "CTRL|SHIFT", action = act.ActivatePaneDirection("Up") },
    { key = "l", mods = "CTRL|SHIFT", action = act.ActivatePaneDirection("Right") },

    { key = "q", mods = "LEADER", action = act.CloseCurrentPane({ confirm = true }) },
    { key = "m", mods = "CTRL|SHIFT", action = act.TogglePaneZoomState },

    { key = "t", mods = "SHIFT|CTRL", action = act.SpawnCommandInNewTab({ cwd = "wezterm.home_dir" }) },
    -- { key = "T", mods = "ALT",    action = act.SpawnCommandInNewTab({ cwd = "wezterm.home_dir" }) },
    -- { key = "t", mods = "SUPER",  action = act.SpawnCommandInNewTab({ domain = "CurrentPaneDomain" }) },
    -- { key = "t", mods = "ALT",    action = act.SpawnCommandInNewTab({ domain = "CurrentPaneDomain" }) },
    --
    { key = "b", mods = "CTRL|SHIFT", action = act.ShowDebugOverlay },

    { key = "p", mods = "CTRL|SHIFT", action = wezterm.action.ActivateTabRelative(-1) },
    { mods = "CTRL|SHIFT", key = "n", action = wezterm.action.ActivateTabRelative(1) },

    -- close tab
    { key = "w", mods = "ALT", action = act.CloseCurrentTab({ confirm = true }) },
    { key = "w", mods = "SUPER", action = act.CloseCurrentTab({ confirm = true }) },

    -- close app
    { key = "q", mods = "SHIFT|CTRL", action = act.QuitApplication },
    { key = "q", mods = "ALT", action = act.QuitApplication },
    { key = "q", mods = "SUPER", action = act.QuitApplication },

    -- reload the configuration
    { key = "r", mods = "SHIFT|CTRL", action = act.ReloadConfiguration },
    { key = "r", mods = "SUPER", action = act.ReloadConfiguration },

    { key = "LeftArrow", mods = "CTRL|ALT", action = wezterm.action.AdjustPaneSize({ "Left", 5 }) },
    { key = "RightArrow", mods = "CTRL|ALT", action = wezterm.action.AdjustPaneSize({ "Right", 5 }) },
    { key = "UpArrow", mods = "CTRL|ALT", action = wezterm.action.AdjustPaneSize({ "Up", 5 }) },
    { key = "DownArrow", mods = "CTRL|ALT", action = wezterm.action.AdjustPaneSize({ "Down", 5 }) },

    -- mode
    -- { key = "r", mods = "LEADER", action = act.ActivateKeyTable({ name = "RESIZE_PANE", one_shot = false }) },
    { key = "r", mods = "LEADER", action = act.ActivateCopyMode },

    { key = "c", mods = "SHIFT|CTRL", action = act.CopyTo("Clipboard") },
    { key = "c", mods = "SHIFT|SUPER", action = act.CopyTo("Clipboard") },
    -- { key = "COPY", mods = "NONE", action = act.CopyTo("Clipboard") },
    { key = "v", mods = "SHIFT|CTRL", action = act.PasteFrom("Clipboard") },
    { key = "v", mods = "SHIFT|SUPER", action = act.PasteFrom("Clipboard") },
    -- { key = "Paste", mods = "NONE", action = act.PasteFrom("Clipboard") },

    -- Font size controls
    { key = "=", mods = "CTRL", action = act.IncreaseFontSize },
    { key = "-", mods = "CTRL", action = act.DecreaseFontSize },
    { key = "0", mods = "CTRL", action = act.ResetFontSize },
  },
  colors = {
    -- Colors for copy_mode and quick_select
    -- available since: 20220807-113146-c2fee766
    -- In copy_mode, the color of the active text is:
    -- 1. copy_mode_active_highlight_* if additional text was selected using the mouse
    -- 2. selection_* otherwise
    copy_mode_active_highlight_bg = { Color = "#000000" },
    -- use `AnsiColor` to specify one of the ansi color palette values
    -- (index 0-15) using one of the names "Black", "Maroon", "Green",
    --  "Olive", "Navy", "Purple", "Teal", "Silver", "Grey", "Red", "Lime",
    -- "Yellow", "Blue", "Fuchsia", "Aqua" or "White".
    copy_mode_active_highlight_fg = { AnsiColor = "Black" },
    copy_mode_inactive_highlight_bg = { Color = "#52ad70" },
    copy_mode_inactive_highlight_fg = { AnsiColor = "White" },
  },

  key_tables = {
    copy_mode = {
      { key = "h", mods = "NONE", action = act.CopyMode("MoveLeft") },
      { key = "j", mods = "NONE", action = act.CopyMode("MoveDown") },
      { key = "k", mods = "NONE", action = act.CopyMode("MoveUp") },
      { key = "l", mods = "NONE", action = act.CopyMode("MoveRight") },

      { key = "0", mods = "NONE", action = act.CopyMode("MoveToStartOfLineContent") },
      { key = "5", mods = "NONE", action = act.CopyMode("MoveToEndOfLineContent") },

      { key = "Escape", mods = "NONE", action = act.CopyMode("Close") },
      { key = "q", mods = "NONE", action = act.CopyMode("Close") },

      { key = "v", mods = "NONE", action = act.CopyMode({ SetSelectionMode = "Cell" }) },
      { key = "v", mods = "CTRL", action = act.CopyMode({ SetSelectionMode = "Block" }) },

      { key = "=", action = act.IncreaseFontSize },
      { key = "-", action = act.DecreaseFontSize },

      {
        key = "y",
        mods = "NONE",
        action = act.Multiple({
          { CopyTo = "ClipboardAndPrimarySelection" },
          { CopyMode = "Close" },
        }),
      },

      -- Scroll up and down by line
      -- { key = 'u', action = act.ScrollByLine(-1) },
      -- { key = 'd', action = act.ScrollByLine(1) },

      -- Scroll up and down by page
      { key = "u", action = act.ScrollByPage(-0.5) },
      { key = "d", action = act.ScrollByPage(0.5) },
    },
  },
}

table.insert(config.hyperlink_rules, {
  regex = [[\b\w+/[\w.-]+\.\w+:\d+\b]],
  format = "$0",
  -- When clicked, will try to open the file
})

local mod = is_macos and "SUPER" or "CTRL"
config.mouse_bindings = {
  -- First, select the word under the mouse
  {
    event = { Down = { streak = 1, button = "Left" } },
    mods = mod,
    action = wezterm.action.SelectTextAtMouseCursor("SemanticZone"),
  },
  -- Then process it
  {
    event = { Up = { streak = 1, button = "Left" } },
    mods = mod,
    action = wezterm.action_callback(function(window, pane)
      -- Get the selected text
      local sel = window:get_selection_text_for_pane(pane)

      -- Match file:line or file:line:col
      local file, line = sel:match("([%w/._-]+%.[%w]+):(%d+)")

      if file and line then
        wezterm.log_info("Found: " .. file .. " line " .. line)
        -- Exit terminal mode with ESC ESC
        pane:send_text("\x1b\x1b")
        -- Wait a moment
        wezterm.sleep_ms(150)
        -- Open the file
        pane:send_text(":e +" .. line .. " " .. file .. "\r")
      end
    end),
  },
}

return config
