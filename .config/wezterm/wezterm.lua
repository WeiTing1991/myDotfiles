local wezterm = require("wezterm")
local act = wezterm.action

-- This will hold the configuration.
if wezterm.config_builder then
	config = wezterm.config_builder()
end

-- Detect the operating system
local is_windows = wezterm.target_triple == "x86_64-pc-windows-msvc"
-- aarch special for m-series mac
local is_macos = wezterm.target_triple == "aarch64-apple-darwin"

local default_prog
local font_size
if is_windows then
  default_prog = { "C:/Program Files/PowerShell/7/pwsh.exe", "-NoLogo" }
  font_size = 12.0
  front_end = "WebGpu"
  webgpu_power_preference = "HighPerformance"
  animation_fps = 1
  max_fps = 120
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
    font = wezterm.font("Consolas", { weight = "Regular" }),
    font_size = 14.0,
  }


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
  window_background_opacity = 0.8,

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
}

config.keys ={
    -- mode
    { key = "x", mods = "LEADER|CTRL", action = act.SendKey({ key = "x", mods = "CTRL" }) },

    -- Split windows
    { key = "phys:Quote", mods = "CTRL|SHIFT", action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
    { key = "5", mods = "CTRL", action = act.SplitHorizontal({ domain = "DefaultDomain" }) },

    { key = "t", mods = "SHIFT|CTRL", action = act.SpawnCommandInNewTab({ cwd = "wezterm.home_dir" }) },

}

return config
