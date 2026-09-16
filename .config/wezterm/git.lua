local wezterm = require("wezterm")

-- Shared git lookups for dispatcher.lua (socket path per repo) and
-- statusbar.lua (branch segment). Both run on hot paths -- the status bar
-- fires every status_update_interval -- so every call goes through a cache.
local M = {}

-- A repo root never changes for a given path, so this never expires.
local root_cache = {}

-- Branch does change, so entries carry a timestamp. Misses are cached too,
-- otherwise every tick inside a non-repo directory shells out again.
local branch_cache = {}
local BRANCH_TTL = 5

-- `git -C <dir>` instead of popen+cd: run_child_process takes no cwd, and this
-- avoids quoting the path into a shell at all.
local function git(dir, ...)
  local args = { "git", "-C", dir, ... }
  local ok, stdout = wezterm.run_child_process(args)
  if not ok then
    return nil
  end
  local out = stdout:gsub("%s+$", "")
  if out == "" then
    return nil
  end
  return out
end

-- Repo root containing `path`, or `path` itself when it isn't in a repo.
function M.root(path)
  local cached = root_cache[path]
  if cached then
    return cached
  end
  local root = git(path, "rev-parse", "--show-toplevel") or path
  root_cache[path] = root
  return root
end

-- Current branch name, short SHA when detached, or nil outside a repo.
function M.branch(path)
  local now = os.time()
  local entry = branch_cache[path]
  if entry and now - entry.at < BRANCH_TTL then
    return entry.name
  end

  local name = git(path, "branch", "--show-current")
  if not name then
    -- Empty output means detached HEAD; no output at all means not a repo.
    -- Only the former has a SHA to fall back to.
    name = git(path, "rev-parse", "--short", "HEAD")
  end

  branch_cache[path] = { name = name, at = now }
  return name
end

return M
