local launch_menu = {}
config.font = wezterm.font_with_fallback({
	-- { family = "RobotoMono Nerd Font", weight = "Regular", italic = false },
	{ family = "Hack Nerd Font", weight = "Regular", italic = false },
	-- { family = "JetBrains Mono", weight = "Regular", italic = false },
})

-- zen mode
wezterm.on("user-var-changed", function(window, pane, name, value)
	local overrides = window:get_config_overrides() or {}
	if name == "ZEN_MODE" then
		local incremental = value:find("+")
		local number_value = tonumber(value)
		if incremental ~= nil then
			while number_value > 0 do
				window:perform_action(wezterm.action.IncreaseFontSize, pane)
				number_value = number_value - 1
			end
			overrides.enable_tab_bar = false
		elseif number_value < 0 then
			window:perform_action(wezterm.action.ResetFontSize, pane)
			overrides.font_size = nil
			overrides.enable_tab_bar = true
		else
			overrides.font_size = number_value
			overrides.enable_tab_bar = false
		end
	end
	window:set_config_overrides(overrides)
end)

--tab bar
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

	local basename = function(s)
		return string.gsub(s, "(.*[/\\])(.*)", "%2")
	end

	-- Current working directory
	local cwd = pane:get_current_working_dir()
	if cwd then
		if type(cwd) == "userdata" then
			cwd = cwd.file_path
		else
			cwd = basename(cwd)
		end
	else
		cwd = ""
	end

	-- Current command
	local cmd = pane:get_foreground_process_name()
	cmd = cmd and basename(cmd) or ""

	window:active_tab():set_title(basename(cwd))

	-- Time
	--local time = wezterm.strftime("%D:%H:%M")

	-- Left status
	window:set_left_status(wezterm.format({
		{ Text = "  " },
	}))

	-- Right status
	window:set_right_status(wezterm.format({
		{ Foreground = { Color = stat_color } },
		{ Text = "  " },
		{ Text = wezterm.nerdfonts.oct_table .. "  " .. stat .. "  " .. window:window_id() },
		{ Text = " | " },
		-- https://wezfurlong.org/wezterm/config/lua/wezterm/nerdfonts.html

		{ Foreground = { Color = "#f6c177" } },
		{ Text = wezterm.nerdfonts.md_folder .. "  " .. cwd },
		{ Text = " | " },

		{ Foreground = { Color = "#f6c177" } },
		{ Text = wezterm.nerdfonts.fa_code .. "  " .. cmd },
		"ResetAttributes",
		{ Text = " | " },
	}))
end)


config.keys = {
	-- copy mode
	{ key = "c", mods = "LEADER", action = act.ActivateCopyMode },
	{ key = "b", mods = "LEADER|CTRL", action = act.SendKey({ key = "b", mods = "CTRL" }) },

	-- workspace
	{
		key = "w",
		mods = "LEADER",
		action = act.PromptInputLine({
			description = wezterm.format({
				{ Attribute = { Intensity = "Bold" } },
				{ Foreground = { AnsiColor = "Fuchsia" } },
				{ Text = "Enter name for new workspace" },
			}),
			action = wezterm.action_callback(function(window, pane, line)
				-- line will be `nil` if they hit escape without entering anything
				-- An empty string if they just hit enter
				-- Or the actual line of text they wrote
				if line then
					window:perform_action(
						act.SwitchToWorkspace({
							name = line,
							cwd = "wezterm.home_dir",
						}),
						pane
					)
				end
			end),
		}),
	},
	{ key = "n", mods = "LEADER", action = act.SwitchWorkspaceRelative(1) },
	{ key = "p", mods = "LEADER", action = act.SwitchWorkspaceRelative(-1) },
	-- { key = "l", mods = "LEADER", action = act.SwitchWorkspace},
	{
		key = "s",
		mods = "LEADER",
		action = act.ShowLauncherArgs({ title = "Workspaces", flags = "FUZZY|WORKSPACES" }),
	},

	-- windows
	-- { key = "'", mods = "LEADER", action = act.SplitVertical({ domain = "DefaultDomain" }) },
	-- { key = "5", mods = "LEADER", action = act.SplitHorizontal({ domain = "DefaultDomain" }) },
	{ key = "h", mods = "LEADER", action = act.ActivatePaneDirection("Left") },
	{ key = "j", mods = "LEADER", action = act.ActivatePaneDirection("Down") },
	{ key = "k", mods = "LEADER", action = act.ActivatePaneDirection("Up") },
	{ key = "l", mods = "LEADER", action = act.ActivatePaneDirection("Right") },
	{ key = "q", mods = "LEADER", action = act.CloseCurrentPane({ confirm = true }) },
	{ key = "z", mods = "LEADER", action = act.TogglePaneZoomState },
	{
		key = "t",
		mods = "LEADER",
		action = wezterm.action_callback(function(_, pane)
			local tab = pane:tab()
			local panes = tab:panes_with_info()
			if #panes == 1 then
				pane:split({
					direction = "Down",
					size = 0.4,
				})
			elseif not panes[1].is_zoomed then
				panes[1].pane:activate()
				tab:set_zoomed(true)
			elseif panes[1].is_zoomed then
				tab:set_zoomed(false)
				panes[2].pane:activate()
			end
		end),
	},
	{ key = "-", mods = "LEADER", action = act.IncreaseFontSize },
	{ key = "=", mods = "LEADER", action = act.DecreaseFontSize },
	{ key = "Enter", mods = "ALT", action = act.ToggleFullScreen },
	{ key = "T", mods = "LEADER", action = act.SpawnCommandInNewTab({ cwd = "wezterm.home_dir" }) },
	{ key = "t", mods = "SUPER", action = act.SpawnCommandInNewTab({ domain = "CurrentPaneDomain" }) },
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
	-- { key = "F1", mols = "LEADER", action = act.ShowDebugOverlay },
	{ key = "r", mods = "LEADER", action = act.ActivateKeyTable({ name = "resize_pane", one_shot = false }) },
	{ key = "m", mods = "LEADER", action = act.ActivateKeyTable({ name = "move_tab", one_shot = false }) },

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

	{ key = "f", mods = "SHIFT|CTRL", action = act.Search("CurrentSelectionOrEmptyString") },
	{ key = "f", mods = "SUPER", action = act.Search("CurrentSelectionOrEmptyString") },
	{ key = "k", mods = "SHIFT|CTRL", action = act.ClearScrollback("ScrollbackOnly") },
	{ key = "k", mods = "SUPER", action = act.ClearScrollback("ScrollbackOnly") },

	{ key = "n", mods = "SHIFT|CTRL", action = act.SpawnWindow },
	{ key = "n", mods = "SUPER", action = act.SpawnWindow },
	--
	{ key = "q", mods = "SHIFT|CTRL", action = act.QuitApplication },
	{ key = "q", mods = "ALT", action = act.QuitApplication },
	{ key = "q", mods = "SUPER", action = act.QuitApplication },
	{ key = "r", mods = "SHIFT|CTRL", action = act.ReloadConfiguration },
	{ key = "r", mods = "SUPER", action = act.ReloadConfiguration },
	{
		key = "u",
		mods = "SHIFT|CTRL",
		action = act.CharSelect({ copy_on_select = true, copy_to = "ClipboardAndPrimarySelection" }),
	},
	{ key = "v", mods = "SHIFT|CTRL", action = act.PasteFrom("Clipboard") },
	{ key = "v", mods = "SUPER", action = act.PasteFrom("Clipboard") },
	-- { key = "w", mods = "SHIFT|CTRL", action = act.CloseCurrentTab({ confirm = true }) },
	-- { key = "w", mods = "ALT", action = act.CloseCurrentTab({ confirm = true }) },
	-- { key = "w", mods = "SUPER", action = act.CloseCurrentTab({ confirm = true }) },
	{ key = "z", mods = "SHIFT|CTRL", action = act.TogglePaneZoomState },
	{ key = "phys:Space", mods = "SHIFT|CTRL", action = act.QuickSelect },
	{ key = "Copy", mods = "NONE", action = act.CopyTo("Clipboard") },
	{ key = "Paste", mods = "NONE", action = act.PasteFrom("Clipboard") },
}

config.key_tables = {

	-- resize_pane = {
	-- 	{ key = "h", action = act.AdjustPaneSize({ "Left", 1 }) },
	-- 	{ key = "j", action = act.AdjustPaneSize({ "Down", 1 }) },
	-- 	{ key = "k", action = act.AdjustPaneSize({ "Up", 1 }) },
	-- 	{ key = "l", action = act.AdjustPaneSize({ "Right", 1 }) },
	-- 	{ key = "Escape", action = "PopKeyTable" },
	-- 	{ key = "+", action = act.IncreaseFontSize },
	-- 	{ key = "-", action = act.DecreaseFontSize },
	-- 	{ key = "Enter", action = "PopKeyTable" },
	-- },

	move_tab = {
		{ key = "h", action = act.MoveTabRelative(-1) },
		{ key = "j", action = act.MoveTabRelative(-1) },
		{ key = "k", action = act.MoveTabRelative(1) },
		{ key = "l", action = act.MoveTabRelative(1) },
		{ key = "Escape", action = "PopKeyTable" },
		{ key = "Enter", action = "PopKeyTable" },
	},

	copy_mode = {
		{ key = "h", mods = "NONE", action = act.CopyMode("MoveLeft") },
		{ key = "j", mods = "NONE", action = act.CopyMode("MoveDown") },
		{ key = "k", mods = "NONE", action = act.CopyMode("MoveUp") },
		{ key = "l", mods = "NONE", action = act.CopyMode("MoveRight") },

		{ key = "Copy", mods = "NONE", action = act.CopyTo("Clipboard") },
		-- { key = "y",     mods = "NONE",  action = act.CopyTo("Clipboard") },
		{ key = "Paste", mods = "NONE", action = act.PasteFrom("Clipboard") },
		{ key = "p", mods = "NONE", action = act.PasteFrom("Clipboard") },
		{ key = "v", mods = "CTRL", action = act.CopyMode({ SetSelectionMode = "Block" }) },
		{ key = "v", mods = "SHIFT", action = act.CopyMode({ SetSelectionMode = "Line" }) },
		{ key = "v", mods = "NONE", action = act.CopyMode({ SetSelectionMode = "Cell" }) },

		{
			key = "y",
			mods = "NONE",
			action = act.Multiple({ { CopyTo = "ClipboardAndPrimarySelection" } }),
		},

		{ key = "Escape", mods = "NONE", action = act.CopyMode("Close") },
		{ key = "c", mods = "CTRL", action = act.CopyMode("Close") },
		{ key = "f", mods = "SHIFT|CTRL", action = act.Search("CurrentSelectionOrEmptyString") },
		{ key = "f", mods = "SUPER", action = act.Search("CurrentSelectionOrEmptyString") },
		{ key = "w", mods = "NONE", action = act.CopyMode("MoveForwardWord") },
		{ key = "b", mods = "NONE", action = act.CopyMode("MoveBackwardWord") },
		{ key = "0", mods = "NONE", action = act.CopyMode("MoveToStartOfLine") },
		{ key = "$", mods = "NONE", action = act.CopyMode("MoveToEndOfLineContent") },
		{ key = "Enter", mods = "NONE", action = act.CopyMode("MoveToStartOfNextLine") },
		{ key = "u", mods = "CTRL", action = act.CopyMode("PageUp") },
		{ key = "d", mods = "CTRL", action = act.CopyMode("PageDown") },
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
