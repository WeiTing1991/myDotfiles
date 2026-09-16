local wezterm = require("wezterm")

local M = {}

local function basename(path)
  return path:match("([^/\\]+)[/\\]?$") or path
end

-- With use_fancy_tab_bar the scheme's [colors.tab_bar] background is ignored --
-- the bar takes its background from window_frame instead, which defaults to a
-- grey that clashes with the Suannhai schemes. Load the active scheme and feed
-- its tab bar background back into the frame.
--
-- Matching on metadata.name rather than mangling the scheme name into a
-- filename, so this keeps working across all eight schemes.
local function find_scheme(dirs, name)
  for _, dir in ipairs(dirs or {}) do
    local listed, entries = pcall(wezterm.read_dir, dir)
    if listed then
      for _, path in ipairs(entries) do
        if path:match("%.toml$") then
          local ok, colors, meta = pcall(wezterm.color.load_scheme, path)
          if ok and meta and meta.name == name then
            return colors
          end
        end
      end
    end
  end
end

function M.apply_to_config(config)
  local colors = find_scheme(config.color_scheme_dirs, config.color_scheme)
  local bg = colors and colors.tab_bar and colors.tab_bar.background
  if bg then
    config.window_frame = config.window_frame or {}
    config.window_frame.active_titlebar_bg = bg
    config.window_frame.inactive_titlebar_bg = bg
  else
    wezterm.log_warn("tabbar: no tab_bar background found for scheme " .. tostring(config.color_scheme))
  end
end

wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
  local title = tab.tab_title
  if not title or #title == 0 then
    local cwd = tab.active_pane.current_working_dir
    title = cwd and basename(cwd.file_path) or tab.active_pane.title
  end

  -- Index matches tab_and_split_indices_are_zero_based, so what's shown here is
  -- what Ctrl+X <n> jumps to.
  local prefix = tab.tab_index .. " "
  if tab.active_pane.is_zoomed then
    prefix = prefix .. "[Z] "
  end

  local suffix = ""
  if #tab.panes > 1 then
    suffix = suffix .. " [" .. #tab.panes .. "]"
  end

  -- Only meaningful on a tab you're not looking at: it means something wrote
  -- output while you were elsewhere.
  if not tab.is_active then
    for _, pane in ipairs(tab.panes) do
      if pane.has_unseen_output then
        suffix = suffix .. " ●"
        break
      end
    end
  end

  local room = max_width - #prefix - #suffix - 2
  if room > 0 then
    title = wezterm.truncate_right(title, room)
  end

  return prefix .. title .. suffix
end)

return M
