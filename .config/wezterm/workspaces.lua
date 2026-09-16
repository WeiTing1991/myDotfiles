local wezterm = require("wezterm")
local act = wezterm.action

-- Project switcher: pick a git repo, get a workspace named after it.
-- Same InputSelector pattern as the tab switcher in wezterm.lua.
local M = {}

local home = wezterm.home_dir

local DEFAULT_ROOTS = {
  home .. "/project",
  home .. "/.dotfiles",
}

local MAX_DEPTH = 3

-- Scanning the disk takes long enough to be felt on a keypress, so the list is
-- built once per wezterm session. Restart wezterm to pick up new repos.
local cache

local function separator(path)
  return path:find("\\") and "\\" or "/"
end

-- Label a repo by its path relative to the root it was found under, so nested
-- repos ("algorithms-journey/cSharpTinyProjects") stay distinguishable and get
-- distinct workspace names.
local function relative_label(path, root)
  local rel = path:sub(#root + 2)
  if rel == "" then
    return path:match("([^/\\]+)[/\\]?$") or path
  end
  return rel
end

-- fd finds repos at any depth. Its output is one ".../.git/" line per repo.
local function scan_with_fd(root)
  local ok, stdout = wezterm.run_child_process({
    "fd", "--hidden", "--type", "d", "--max-depth", tostring(MAX_DEPTH),
    "--glob", ".git", root,
  })
  if not ok then
    return nil
  end
  local repos = {}
  for line in stdout:gmatch("[^\r\n]+") do
    table.insert(repos, (line:gsub("[/\\]%.git[/\\]?$", "")))
  end
  return repos
end

-- Without fd, fall back to the immediate subdirectories. Flatter, but it keeps
-- the picker working on a machine that only has wezterm installed.
local function scan_with_read_dir(root)
  local ok, entries = pcall(wezterm.read_dir, root)
  if not ok then
    return {}
  end
  return entries
end

local function projects()
  if cache then
    return cache
  end

  local seen, choices = {}, {}
  for _, root in ipairs(DEFAULT_ROOTS) do
    local repos = scan_with_fd(root) or scan_with_read_dir(root)
    for _, path in ipairs(repos) do
      if not seen[path] then
        seen[path] = true
        table.insert(choices, { label = relative_label(path, root), id = path })
      end
    end
  end

  table.sort(choices, function(a, b) return a.label < b.label end)
  cache = choices
  return cache
end

-- Open (or switch to) a workspace per project.
local function pick_project(window, pane)
  local choices = projects()
  if #choices == 0 then
    window:toast_notification("wezterm", "No projects found", nil, 4000)
    return
  end

  window:perform_action(
    act.InputSelector({
      title = "Open Project",
      choices = choices,
      fuzzy = true,
      action = wezterm.action_callback(function(w, p, id, label)
        if not id then return end
        w:perform_action(
          act.SwitchToWorkspace({ name = label, spawn = { cwd = id } }),
          p
        )
      end),
    }),
    pane
  )
end

-- Switch between workspaces that are already open.
local function pick_workspace(window, pane)
  local choices = {}
  for _, name in ipairs(wezterm.mux.get_workspace_names()) do
    table.insert(choices, { label = name, id = name })
  end

  window:perform_action(
    act.InputSelector({
      title = "Switch Workspace",
      choices = choices,
      fuzzy = true,
      action = wezterm.action_callback(function(w, p, id)
        if not id then return end
        w:perform_action(act.SwitchToWorkspace({ name = id }), p)
      end),
    }),
    pane
  )
end

-- Exposed for inspection from the debug overlay.
M.projects = projects

M.keys = {
  { key = "w", mods = "LEADER", action = wezterm.action_callback(pick_project) },
  { key = "W", mods = "LEADER", action = wezterm.action_callback(pick_workspace) },
}

return M
