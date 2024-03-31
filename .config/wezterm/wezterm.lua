-- good to chekout: the mux
local wezterm = require("wezterm")
local act = wezterm.action
-- This will hold the configuration.
local config = {}
if wezterm.config_builder then
	config = wezterm.config_builder()
end
local launch_menu = {}
-- for windows
--- Set Pwsh as the default on Windows
-- This is where you actually apply your config choices
config.font = wezterm.font_with_fallback({
	{ family = "Hack Nerd Font", weight = "Medium", italic = false },
	--{ family = "JetBrains Mono", weight = "Regular", italic = false },
})
config.launch_menu = launch_menu
config.color_scheme = "carbonfox"
config.font_size = 14.0
--config.adjust_window_size_when_changing_font_size = true
config.window_background_opacity = 0.75
config.macos_window_background_blur = 50
config.window_close_confirmation = "AlwaysPrompt"
-- windows
-- window
config.initial_cols = 120
config.initial_rows = 40
config.inactive_pane_hsb = {
	saturation = 0.5,
	brightness = 0.8,
}
config.window_padding = {
	left = 14,
	right = 14,
	top = 5,
	bottom = 5,
}
config.window_decorations = "RESIZE"

config.use_fancy_tab_bar = false
config.status_update_interval = 1000
config.tab_bar_at_bottom = false

-- keys
-- disable default keybindings
config.disable_default_key_bindings = true
config.leader = { key = "b", mods = "CTRL", timeout_milliseconds = 5000 }

--tab bar
wezterm.on("update-status", function(window, pane)
	--window:set_position(0, 0)
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

	local basename = function(s)
		return string.gsub(s, "(.*[/\\])(.*)", "%2")
	end

	-- Current working directory
	local cwd = pane:get_current_working_dir()
	if cwd then
		if type(cwd) == "userdata" then
			cwd = basename(cwd.file_path)
		else
			cwd = basename(cwd)
		end
	else
		cwd = ""
	end

	-- Current command
	local cmd = pane:get_foreground_process_name()
	cmd = cmd and basename(cmd) or ""

	window:active_tab():set_title(cwd)
	-- Time
	local time = wezterm.strftime("%D:%H:%M")

	-- Left status
	window:set_left_status(wezterm.format({
		{ Foreground = { Color = stat_color } },
		{ Text = "  " },
		{ Text = wezterm.nerdfonts.oct_table .. "  " .. stat .. "  " .. window:window_id() },
		{ Text = " | " },
	}))

	-- Right status
	window:set_right_status(wezterm.format({
		-- https://wezfurlong.org/wezterm/config/lua/wezterm/nerdfonts.html
		{ Text = wezterm.nerdfonts.md_folder .. "  " .. cwd },
		{ Text = " | " },
		{ Foreground = { Color = "#f6c177" } },
		{ Text = wezterm.nerdfonts.fa_code .. "  " .. cmd },
		"ResetAttributes",
		{ Text = " | " },
		{ Text = wezterm.nerdfonts.md_clock .. "  " .. time },
		{ Text = "  " },
	}))
end)

-- keys
-- disable default keybindings
config.leader = { key = "b", mods = "CTRL", timeout_milliseconds = 5000 }

config.keys = {
	-- copy mode
	{ key = "c", mods = "LEADER", action = act.ActivateCopyMode },
	{ key = "b", mods = "LEADER|CTRL", action = act.SendKey({ key = "b", mods = "CTRL" }) },
	-- windows
	{ key = "'", mods = "LEADER", action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
	{ key = "5", mods = "LEADER", action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
	{ key = "h", mods = "LEADER", action = act.ActivatePaneDirection("Left") },
	{ key = "j", mods = "LEADER", action = act.ActivatePaneDirection("Down") },
	{ key = "k", mods = "LEADER", action = act.ActivatePaneDirection("Up") },
	{ key = "l", mods = "LEADER", action = act.ActivatePaneDirection("Right") },
	{ key = "q", mods = "LEADER", action = act.CloseCurrentPane({ confirm = true }) },
	{ key = "t", mods = "LEADER", action = act.TogglePaneZoomState },

	{ key = "_", mods = "LEADER", action = act.IncreaseFontSize },
	{ key = "+", mods = "LEADER", action = act.DecreaseFontSize },
	{ key = "Enter", mods = "ALT", action = act.ToggleFullScreen },
	{ key = "T", mods = "LEADER", action = act.SpawnTab("DefaultDomain") },
	{
		key = "e",
		mods = "LEADER",
		action = act.PromptInputLine({
			description = wezterm.format({
				{ Attribute = { Intensity = "Bold" } },
				{ Foreground = { Color = "#eb6f92" } },
				{ Text = "Renaming Tab Title...:" },
			}),
			action = wezterm.action_callback(function(window, pane, line)
				if line then
					window:active_tab():set_title(line)
				end
			end),
		}),
	},
	-- mode
	{ key = "F1", mods = "LEADER", action = act.ShowDebugOverlay },
	{ key = "r", mods = "LEADER", action = act.ActivateKeyTable({ name = "resize_pane", one_shot = false }) },
	{ key = "m", mods = "LEADER", action = act.ActivateKeyTable({ name = "move_tab", one_shot = false }) },
	-- { key = '!', mods = 'CTRL', action = act.ActivateTab(0) },
	-- { key = '!', mods = 'SHIFT|CTRL', action = act.ActivateTab(0) },
	-- { key = '#', mods = 'CTRL', action = act.ActivateTab(2) },
	-- { key = '#', mods = 'SHIFT|CTRL', action = act.ActivateTab(2) },
	-- { key = '$', mods = 'CTRL', action = act.ActivateTab(3) },
	-- { key = '$', mods = 'SHIFT|CTRL', action = act.ActivateTab(3) },
	-- { key = '%', mods = 'CTRL', action = act.ActivateTab(4) },
	-- { key = '%', mods = 'SHIFT|CTRL', action = act.ActivateTab(4) },
	-- { key = '&', mods = 'CTRL', action = act.ActivateTab(6) },
	-- { key = '&', mods = 'SHIFT|CTRL', action = act.ActivateTab(6) },
	-- { key = '(', mods = 'CTRL', action = act.ActivateTab(-1) },
	-- { key = '(', mods = 'SHIFT|CTRL', action = act.ActivateTab(-1) },
	-- { key = ')', mods = 'CTRL', action = act.ResetFontSize },
	-- { key = ')', mods = 'SHIFT|CTRL', action = act.ResetFontSize },
	-- { key = '*', mods = 'CTRL', action = act.ActivateTab(7) },
	-- { key = '*', mods = 'SHIFT|CTRL', action = act.ActivateTab(7) },

	{ key = "1", mods = "SHIFT|CTRL", action = act.ActivateTab(0) },
	{ key = "1", mods = "SUPER", action = act.ActivateTab(0) },
	{ key = "2", mods = "SHIFT|CTRL", action = act.ActivateTab(1) },
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
	{ key = "C", mods = "CTRL", action = act.CopyTo("Clipboard") },
	{ key = "C", mods = "SHIFT|CTRL", action = act.CopyTo("Clipboard") },
	{ key = "F", mods = "CTRL", action = act.Search("CurrentSelectionOrEmptyString") },
	{ key = "F", mods = "SHIFT|CTRL", action = act.Search("CurrentSelectionOrEmptyString") },

	{ key = "1", mods = "ALT", action = act.ActivateTab(0) },
	{ key = "2", mods = "ALT", action = act.ActivateTab(1) },
	{ key = "3", mods = "ALT", action = act.ActivateTab(2) },
	{ key = "4", mods = "ALT", action = act.ActivateTab(3) },
	{ key = "5", mods = "ALT", action = act.ActivateTab(4) },
	{ key = "6", mods = "ALT", action = act.ActivateTab(5) },
	{ key = "7", mods = "ALT", action = act.ActivateTab(6) },
	{ key = "8", mods = "ALT", action = act.ActivateTab(7) },
	{ key = "9", mods = "ALT", action = act.ActivateTab(-1) },

	-- { key = 'H', mods = 'CTRL', action = act.HideApplication },
	-- { key = 'H', mods = 'SHIFT|CTRL', action = act.HideApplication },
	-- { key = 'K', mods = 'CTRL', action = act.ClearScrollback 'ScrollbackOnly' },
	-- { key = 'K', mods = 'SHIFT|CTRL', action = act.ClearScrollback 'ScrollbackOnly' },
	-- { key = 'L', mods = 'CTRL', action = act.ShowDebugOverlay },
	-- { key = 'L', mods = 'SHIFT|CTRL', action = act.ShowDebugOverlay },
	-- { key = 'M', mods = 'CTRL', action = act.Hide },

	-- { key = 'Q', mods = 'CTRL', action = act.QuitApplication },
	-- { key = 'Q', mods = 'SHIFT|CTRL', action = act.QuitApplication },
	-- { key = 'R', mods = 'CTRL', action = act.ReloadConfiguration },
	-- { key = 'R', mods = 'SHIFT|CTRL', action = act.ReloadConfiguration },
	-- { key = 'U', mods = 'CTRL', action = act.CharSelect{ copy_on_select = true, copy_to =  'ClipboardAndPrimarySelection' } },
	-- { key = 'U', mods = 'SHIFT|CTRL', action = act.CharSelect{ copy_on_select = true, copy_to =  'ClipboardAndPrimarySelection' } },
	-- { key = 'V', mods = 'CTRL', action = act.PasteFrom 'Clipboard' },
	-- { key = 'V', mods = 'SHIFT|CTRL', action = act.PasteFrom 'Clipboard' },
	-- { key = 'W', mods = 'CTRL', action = act.CloseCurrentTab{ confirm = true } },
	-- { key = 'W', mods = 'SHIFT|CTRL', action = act.CloseCurrentTab{ confirm = true } },
	-- { key = 'X', mods = 'CTRL', action = act.ActivateCopyMode },
	-- { key = 'X', mods = 'SHIFT|CTRL', action = act.ActivateCopyMode },
	-- { key = 'Z', mods = 'CTRL', action = act.TogglePaneZoomState },
	-- { key = 'Z', mods = 'SHIFT|CTRL', action = act.TogglePaneZoomState },
	-- { key = '[', mods = 'SHIFT|SUPER', action = act.ActivateTabRelative(-1) },
	-- { key = ']', mods = 'SHIFT|SUPER', action = act.ActivateTabRelative(1) },
	-- { key = '^', mods = 'CTRL', action = act.ActivateTab(5) },
	-- { key = '^', mods = 'SHIFT|CTRL', action = act.ActivateTab(5) },
	-- { key = '_', mods = 'CTRL', action = act.DecreaseFontSize },
	-- { key = '_', mods = 'SHIFT|CTRL', action = act.DecreaseFontSize },

	{ key = "f", mods = "SHIFT|CTRL", action = act.Search("CurrentSelectionOrEmptyString") },
	{ key = "f", mods = "SUPER", action = act.Search("CurrentSelectionOrEmptyString") },
	{ key = "k", mods = "SHIFT|CTRL", action = act.ClearScrollback("ScrollbackOnly") },
	{ key = "k", mods = "SUPER", action = act.ClearScrollback("ScrollbackOnly") },

	-- { key = "n", mods = "SHIFT|CTRL", action = act.SpawnWindow },
	-- { key = "n", mods = "SUPER", action = act.SpawnWindow },
	-- { key = "p", mods = "SHIFT|CTRL", action = act.ActivateCommandPalette },
	--
	{ key = "q", mods = "SHIFT|CTRL", action = act.QuitApplication },
	{ key = "q", mods = "ALT", action = act.QuitApplication },
	{ key = "q", mods = "SUPER", action = act.QuitApplication },
	{ key = "r", mods = "SHIFT|CTRL", action = act.ReloadConfiguration },
	{ key = "r", mods = "SUPER", action = act.ReloadConfiguration },
	{ key = "t", mods = "SHIFT|CTRL", action = act.SpawnTab("DefaultDomain") },
	{ key = "t", mods = "ALT", action = act.SpawnTab("DefaultDomain") },
	{ key = "t", mods = "SUPER", action = act.SpawnTab("DefaultDomain") },
	{
		key = "u",
		mods = "SHIFT|CTRL",
		action = act.CharSelect({ copy_on_select = true, copy_to = "ClipboardAndPrimarySelection" }),
	},
	{ key = "v", mods = "SHIFT|CTRL", action = act.PasteFrom("Clipboard") },
	{ key = "v", mods = "SUPER", action = act.PasteFrom("Clipboard") },
	{ key = "w", mods = "SHIFT|CTRL", action = act.CloseCurrentTab({ confirm = true }) },
	{ key = "w", mods = "ALT", action = act.CloseCurrentTab({ confirm = true }) },
	{ key = "w", mods = "SUPER", action = act.CloseCurrentTab({ confirm = true }) },
	{ key = "z", mods = "SHIFT|CTRL", action = act.TogglePaneZoomState },
	{ key = "phys:Space", mods = "SHIFT|CTRL", action = act.QuickSelect },
	{ key = "PageUp", mods = "SHIFT", action = act.ScrollByPage(-1) },
	{ key = "PageUp", mods = "CTRL", action = act.ActivateTabRelative(-1) },
	{ key = "PageUp", mods = "SHIFT|CTRL", action = act.MoveTabRelative(-1) },
	{ key = "PageDown", mods = "SHIFT", action = act.ScrollByPage(1) },
	{ key = "PageDown", mods = "CTRL", action = act.ActivateTabRelative(1) },
	{ key = "PageDown", mods = "SHIFT|CTRL", action = act.MoveTabRelative(1) },
	{ key = "Copy", mods = "NONE", action = act.CopyTo("Clipboard") },
	{ key = "Paste", mods = "NONE", action = act.PasteFrom("Clipboard") },
}

config.key_tables = {

	resize_pane = {
		{ key = "h", action = act.AdjustPaneSize({ "Left", 1 }) },
		{ key = "j", action = act.AdjustPaneSize({ "Down", 1 }) },
		{ key = "k", action = act.AdjustPaneSize({ "Up", 1 }) },
		{ key = "l", action = act.AdjustPaneSize({ "Right", 1 }) },
		{ key = "Escape", action = "PopKeyTable" },
		{ key = "_", action = act.IncreaseFontSize },
		{ key = "+", action = act.DecreaseFontSize },
		{ key = "Enter", action = "PopKeyTable" },
	},
	move_tab = {
		{ key = "h", action = act.MoveTabRelative(-1) },
		{ key = "j", action = act.MoveTabRelative(-1) },
		{ key = "k", action = act.MoveTabRelative(1) },
		{ key = "l", action = act.MoveTabRelative(1) },
		{ key = "Escape", action = "PopKeyTable" },
		{ key = "Enter", action = "PopKeyTable" },
	},
	-- TODO
	copy_mode = {
		{ key = "h", mods = "NONE", action = act.CopyMode("MoveLeft") },
		{ key = "j", mods = "NONE", action = act.CopyMode("MoveDown") },
		{ key = "k", mods = "NONE", action = act.CopyMode("MoveUp") },
		{ key = "l", mods = "NONE", action = act.CopyMode("MoveRight") },

		{ key = "Copy", mods = "NONE", action = act.CopyTo("Clipboard") },
		{ key = "y", mods = "NONE", action = act.CopyTo("Clipboard") },
		{ key = "Paste", mods = "NONE", action = act.PasteFrom("Clipboard") },
		{ key = "p", mods = "NONE", action = act.PasteFrom("Clipboard") },
		{ key = "V", mods = "NONE", action = act.CopyMode({ SetSelectionMode = "Line" }) },
		{ key = "V", mods = "SHIFT", action = act.CopyMode({ SetSelectionMode = "Line" }) },

		{ key = "Escape", mods = "NONE", action = act.CopyMode("Close") },

		-- { key = "Tab", mods = "NONE", action = act.CopyMode("MoveForwardWord") },
		-- { key = "Tab", mods = "SHIFT", action = act.CopyMode("MoveBackwardWord") },
		-- { key = "Enter", mods = "NONE", action = act.CopyMode("MoveToStartOfNextLine") },
		-- { key = "Escape", mods = "NONE", action = act.CopyMode("Close") },
		-- { key = "Space", mods = "NONE", action = act.CopyMode({ SetSelectionMode = "Cell" }) },
		-- { key = "$", mods = "NONE", action = act.CopyMode("MoveToEndOfLineContent") },
		-- { key = "$", mods = "SHIFT", action = act.CopyMode("MoveToEndOfLineContent") },
		-- { key = ",", mods = "NONE", action = act.CopyMode("JumpReverse") },
		-- { key = "0", mods = "NONE", action = act.CopyMode("MoveToStartOfLine") },
		-- { key = ";", mods = "NONE", action = act.CopyMode("JumpAgain") },
		-- { key = "F", mods = "NONE", action = act.CopyMode({ JumpBackward = { prev_char = false } }) },
		-- { key = "F", mods = "SHIFT", action = act.CopyMode({ JumpBackward = { prev_char = false } }) },
		-- { key = "G", mods = "NONE", action = act.CopyMode("MoveToScrollbackBottom") },
		-- { key = "G", mods = "SHIFT", action = act.CopyMode("MoveToScrollbackBottom") },
		-- { key = "H", mods = "NONE", action = act.CopyMode("MoveToViewportTop") },
		-- { key = "H", mods = "SHIFT", action = act.CopyMode("MoveToViewportTop") },
		-- { key = "L", mods = "NONE", action = act.CopyMode("MoveToViewportBottom") },
		-- { key = "L", mods = "SHIFT", action = act.CopyMode("MoveToViewportBottom") },
		-- { key = "M", mods = "NONE", action = act.CopyMode("MoveToViewportMiddle") },
		-- { key = "M", mods = "SHIFT", action = act.CopyMode("MoveToViewportMiddle") },
		-- { key = "O", mods = "NONE", action = act.CopyMode("MoveToSelectionOtherEndHoriz") },
		-- { key = "O", mods = "SHIFT", action = act.CopyMode("MoveToSelectionOtherEndHoriz") },
		-- { key = "T", mods = "NONE", action = act.CopyMode({ JumpBackward = { prev_char = true } }) },
		-- { key = "T", mods = "SHIFT", action = act.CopyMode({ JumpBackward = { prev_char = true } }) },
		-- { key = "^", mods = "NONE", action = act.CopyMode("MoveToStartOfLineContent") },
		-- { key = "^", mods = "SHIFT", action = act.CopyMode("MoveToStartOfLineContent") },
		-- { key = "b", mods = "NONE", action = act.CopyMode("MoveBackwardWord") },
		-- { key = "b", mods = "ALT", action = act.CopyMode("MoveBackwardWord") },
		-- { key = "b", mods = "CTRL", action = act.CopyMode("PageUp") },
		-- { key = "c", mods = "CTRL", action = act.CopyMode("Close") },
		-- { key = "d", mods = "CTRL", action = act.CopyMode({ MoveByPage = 0.5 }) },
		-- { key = "e", mods = "NONE", action = act.CopyMode("MoveForwardWordEnd") },
		-- { key = "f", mods = "NONE", action = act.CopyMode({ JumpForward = { prev_char = false } }) },
		-- { key = "f", mods = "ALT", action = act.CopyMode("MoveForwardWord") },
		-- { key = "f", mods = "CTRL", action = act.CopyMode("PageDown") },
		-- { key = "g", mods = "NONE", action = act.CopyMode("MoveToScrollbackTop") },
		-- { key = "g", mods = "CTRL", action = act.CopyMode("Close") },
		--
		-- { key = "m", mods = "ALT", action = act.CopyMode("MoveToStartOfLineContent") },
		-- { key = "o", mods = "NONE", action = act.CopyMode("MoveToSelectionOtherEnd") },
		-- { key = "q", mods = "NONE", action = act.CopyMode("Close") },
		-- { key = "t", mods = "NONE", action = act.CopyMode({ JumpForward = { prev_char = true } }) },
		-- { key = "u", mods = "CTRL", action = act.CopyMode({ MoveByPage = -0.5 }) },
		-- { key = "v", mods = "NONE", action = act.CopyMode({ SetSelectionMode = "Cell" }) },
		-- { key = "v", mods = "CTRL", action = act.CopyMode({ SetSelectionMode = "Block" }) },
		-- { key = "w", mods = "NONE", action = act.CopyMode("MoveForwardWord") },
		-- {
		-- 	key = "y",
		-- 	mods = "NONE",
		-- 	action = act.Multiple({ { CopyTo = "ClipboardAndPrimarySelection" }, { CopyMode = "Close" } }),
		-- },
		-- { key = "PageUp", mods = "NONE", action = act.CopyMode("PageUp") },
		-- { key = "PageDown", mods = "NONE", action = act.CopyMode("PageDown") },
		-- { key = "End", mods = "NONE", action = act.CopyMode("MoveToEndOfLineContent") },
		-- { key = "Home", mods = "NONE", action = act.CopyMode("MoveToStartOfLine") },
		-- { key = "LeftArrow", mods = "NONE", action = act.CopyMode("MoveLeft") },
		-- { key = "LeftArrow", mods = "ALT", action = act.CopyMode("MoveBackwardWord") },
		-- { key = "RightArrow", mods = "NONE", action = act.CopyMode("MoveRight") },
		-- { key = "RightArrow", mods = "ALT", action = act.CopyMode("MoveForwardWord") },
		-- { key = "UpArrow", mods = "NONE", action = act.CopyMode("MoveUp") },
		-- { key = "DownArrow", mods = "NONE", action = act.CopyMode("MoveDown") },
	},

	search_mode = {
		{ key = "Enter", mods = "NONE", action = act.CopyMode("PriorMatch") },
		{ key = "Escape", mods = "NONE", action = act.CopyMode("Close") },
		{ key = "n", mods = "CTRL", action = act.CopyMode("NextMatch") },
		{ key = "p", mods = "CTRL", action = act.CopyMode("PriorMatch") },
		{ key = "r", mods = "CTRL", action = act.CopyMode("CycleMatchType") },
		{ key = "u", mods = "CTRL", action = act.CopyMode("ClearPattern") },
		{ key = "PageUp", mods = "NONE", action = act.CopyMode("PriorMatchPage") },
		{ key = "PageDown", mods = "NONE", action = act.CopyMode("NextMatchPage") },
		{ key = "UpArrow", mods = "NONE", action = act.CopyMode("PriorMatch") },
		{ key = "DownArrow", mods = "NONE", action = act.CopyMode("NextMatch") },
	},
}

return config
