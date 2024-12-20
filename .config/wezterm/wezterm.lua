local wezterm = require("wezterm")
local act = wezterm.action

-- This will hold the configuration.
if wezterm.config_builder then
	config = wezterm.config_builder()
end

-- Detect the operating system
local is_windows = wezterm.target_triple == "x86_64-pc-windows-msvc"
local is_macos = wezterm.target_triple == "aarch64-apple-darwin"

local default_prog
local font_size
if is_windows then
  default_prog = { "C:/Program Files/PowerShell/7/pwsh.exe", "-NoLogo" }
  font_size = 12.0
  front_end = "OpenGL"
elseif is_macos then
  default_prog = { "/bin/zsh" } -- Example: Zsh for macOS
  font_size = 16.0
end


-- config settings
config = {
  default_prog = default_prog,
  -- GUI
  max_fps = 144,
	-- animation_fps = 120,
  -- color
  color_scheme = "rose-pine",

  colors = {
  	foreground = "silver",
  	background = "#0D0907",
  	selection_fg = "#20211A",
  	selection_bg = "silver",
  	-- tab_bar = {
  	-- 	background = "#0D0907",
  	-- 	active_tab = {
  	-- 		bg_color = "#0D0907",
  	-- 		fg_color = "silver",
  	-- 		intensity = "Bold",
  	-- 	},
  	-- 	inactive_tab = {
  	-- 		bg_color = "#20211A",
  	-- 		fg_color = "silver",
  	-- 		intensity = "Half",
  	-- 	},
  	-- },
  },

  -- font settings
  font_size = font_size,
  font = wezterm.font(
	 "Hack Nerd Font", {weight = "Regular", italic = false }
  ),


-- Windows
--initial_cols = 120,
--initial_rows = 40,
inactive_pane_hsb = {
	saturation = 0.5,
	brightness = 0.8,
},
window_decorations = "RESIZE",
window_padding = {
	left = 8,
	right = 8,
	top = 8,
	bottom = 8,
},

adjust_window_size_when_changing_font_size = true,
window_background_opacity = 0.95,
macos_window_background_blur = 100,
window_close_confirmation = "AlwaysPrompt",
default_cursor_style = "BlinkingBlock",

-- tab bar
hide_tab_bar_if_only_one_tab = false,
tab_bar_at_bottom = true,
use_fancy_tab_bar = false,
tab_and_split_indices_are_zero_based = true,

wezterm.on("update-status", function(window, pane)
	-- Workspace name
	local stat = window:active_workspace()
	local stat_color = "#eb6f92"

	if window:active_key_table() then
		stat = window:active_key_table()
		stat_color = "#e0def4"
	end
	if window:leader_is_active() then
		stat = "TERMINAL"
		stat_color = "#31748f"
	end

	-- Left status
	window:set_left_status(wezterm.format({
		{ Text = "" },
	}))

	-- Right status
	window:set_right_status(wezterm.format({
		{ Foreground = { Color = stat_color } },
		-- { background = { Color = "#0D0907" - },
		{ Text = "  " },
		{ Text = stat .. "  " .. window:window_id() },
		-- { Text = " | " },
		-- { Foreground = { Color = "#f6c177" } },
		-- { Text = cwd },
		-- "ResetAttributes",
	}))
end),

-- Disable default keybindings
disable_default_key_bindings = true,
leader = { key = "x", mods = "CTRL", timeout_milliseconds = 1000 },

keys = {
	-- mode
	-- { key = "x", mods = "LEADER|CTRL", action = act.SendKey({ key = "x", mods = "CTRL" }) },

	-- split windows
  { key = "'", mods = "LEADER", action = act.SplitVertical({ domain = "DefaultDomain" }) },
	{ key = "5", mods = "LEADER", action = act.SplitHorizontal({ domain = "DefaultDomain" }) },

  -- Pane navigation
	{ key = "h", mods = "LEADER", action = act.ActivatePaneDirection("Left") },
	{ key = "j", mods = "LEADER", action = act.ActivatePaneDirection("Down") },
	{ key = "k", mods = "LEADER", action = act.ActivatePaneDirection("Up") },
	{ key = "l", mods = "LEADER", action = act.ActivatePaneDirection("Right") },

	{ key = "q", mods = "LEADER", action = act.CloseCurrentPane({ confirm = true }) },
	{ key = "t", mods = "LEADER", action = act.TogglePaneZoomState },

	{ key = "T", mods = "SUPER", action = act.SpawnCommandInNewTab({ cwd = "wezterm.home_dir" }) },
	{ key = "T", mods = "ALT", action = act.SpawnCommandInNewTab({ cwd = "wezterm.home_dir" }) },
	{ key = "t", mods = "SUPER", action = act.SpawnCommandInNewTab({ domain = "CurrentPaneDomain" }) },
	{ key = "t", mods = "ALT", action = act.SpawnCommandInNewTab({ domain = "CurrentPaneDomain" }) },

  {
      key = "p",
      mods = "LEADER",
      action = wezterm.action.ActivateTabRelative(-1)
  },
  {
      mods = "LEADER",
      key = "n",
      action = wezterm.action.ActivateTabRelative(1)
  },

	{ key = "1", mods = "ALT", action = act.ActivateTab(0) },
	{ key = "1", mods = "SUPER", action = act.ActivateTab(0) },
	{ key = "2", mods = "ALT", action = act.ActivateTab(1) },
	{ key = "2", mods = "SUPER", action = act.ActivateTab(1) },
	{ key = "3", mods = "SHIFT|CTRL", action = act.ActivateTab(2) },
	{ key = "3", mods = "SUPER", action = act.ActivateTab(2) },
	{ key = "4", mods = "SHIFT|CTRL", action = act.ActivateTab(3) },
	{ key = "4", mods = "SUPER", action = act.ActivateTab(3) },
	{ key = "5", mods = "SHIFT|CTRL", action = act.ActivateTab(4) },
	{ key = "5", mods = "SUPER", action = act.ActivateTab(4) },
	{ key = "6", mods = "SHIFT|CTRL", action = act.ActivateTab(5) },
	{ key = "6", mods = "SUPER", action = act.ActivateTab(5) },
	{ key = "7", mods = "SHIFT|CTRL", action = act.ActivateTab(6) },
	{ key = "7", mods = "SUPER", action = act.ActivateTab(6) },
	{ key = "8", mods = "SHIFT|CTRL", action = act.ActivateTab(7) },
	{ key = "8", mods = "SUPER", action = act.ActivateTab(7) },
	{ key = "9", mods = "SHIFT|CTRL", action = act.ActivateTab(-1) },
	{ key = "9", mods = "SUPER", action = act.ActivateTab(-1) },

	-- { key = "w", mods = "SHIFT|CTRL", action = act.CloseCurrentTab({ confirm = true }) },

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

  -- mode
	{ key = "r", mods = "LEADER", action = act.ActivateKeyTable({ name = "RESIZE_PANE", one_shot = false }) },
	{ key = "Copy", mods = "NONE", action = act.CopyTo("Clipboard") },
	{ key = "c", mods = "SHIFT|CTRL", action = act.CopyTo("Clipboard") },
	{ key = "Paste", mods = "NONE", action = act.PasteFrom("Clipboard") },
	{ key = "v", mods = "SHIFT|CTRL", action = act.PasteFrom("Clipboard") },
},

key_tables = {
	RESIZE_PANE = {
		{ key = "h", action = act.AdjustPaneSize({ "Left", 1 }) },
		{ key = "j", action = act.AdjustPaneSize({ "Down", 1 }) },
		{ key = "k", action = act.AdjustPaneSize({ "Up", 1 }) },
		{ key = "l", action = act.AdjustPaneSize({ "Right", 1 }) },
		{ key = "Escape", action = "PopKeyTable" },
		{ key = "Enter", action = "PopKeyTable" },
		{ key = "=", action = act.IncreaseFontSize },
		{ key = "-", action = act.DecreaseFontSize },
	},
},

}

return config
