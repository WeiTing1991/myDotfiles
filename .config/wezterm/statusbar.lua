local wezterm = require("wezterm")
local git = require("git")

-- Right status: active mode, workspace, git branch, cwd.
-- Uses "update-status"; "update-right-status" is deprecated since
-- 20220903-194523-3bb1ed61.
local M = {}

local KEY_TABLE_LABELS = {
  resize_pane = "RESIZE",
  copy_mode = "COPY",
  search_mode = "SEARCH",
}

local SEPARATOR = "  "

-- effective_config() rebuilds a table on every call and this runs once per
-- status_update_interval, so hold the palette until the config is reloaded.
local palette_cache = {}

wezterm.on("window-config-reloaded", function(window)
  palette_cache[window:window_id()] = nil
end)

local function palette(window)
  local id = window:window_id()
  local cached = palette_cache[id]
  if not cached then
    cached = window:effective_config().resolved_palette
    palette_cache[id] = cached
  end
  return cached
end

local function basename(path)
  return path:match("([^/\\]+)[/\\]?$") or path
end

local function push(cells, color, text)
  if #cells > 0 then
    table.insert(cells, { Text = SEPARATOR })
  end
  table.insert(cells, { Foreground = { Color = color } })
  table.insert(cells, { Text = text })
end

wezterm.on("update-status", function(window, pane)
  local colors = palette(window)
  local ansi = colors.ansi or {}
  local brights = colors.brights or {}
  local fg = colors.foreground

  local cells = {}

  -- Mode. Highest-value segment here: disable_default_key_bindings is on, so
  -- there is no other cue that the leader or a key table is armed.
  local mode = window:leader_is_active() and "LEADER"
    or KEY_TABLE_LABELS[window:active_key_table() or ""]
  if mode then
    table.insert(cells, { Foreground = { Color = colors.background } })
    table.insert(cells, { Background = { Color = ansi[4] or fg } })
    table.insert(cells, { Attribute = { Intensity = "Bold" } })
    table.insert(cells, { Text = " " .. mode .. " " })
    table.insert(cells, "ResetAttributes")
  end

  local workspace = window:active_workspace()
  if workspace and workspace ~= "" then
    push(cells, ansi[6] or fg, workspace)
  end

  local cwd = pane:get_current_working_dir()
  if cwd then
    local branch = git.branch(cwd.file_path)
    if branch then
      push(cells, ansi[3] or fg, " " .. branch)
    end
    push(cells, brights[8] or fg, basename(cwd.file_path))
  end

  table.insert(cells, { Text = " " })
  window:set_right_status(wezterm.format(cells))
end)

return M
