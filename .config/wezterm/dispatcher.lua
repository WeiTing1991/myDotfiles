local wezterm = require("wezterm")

local M = {}

local is_windows = wezterm.target_triple:find("windows") ~= nil

local root_cache = {}

local function git_root(path)
  if root_cache[path] then return root_cache[path] end
  local devnull = is_windows and "NUL" or "/dev/null"
  local p = io.popen('cd ' .. ('%q'):format(path) ..
                     ' && git rev-parse --show-toplevel 2>' .. devnull)
  local root = p and p:read('*l')
  if p then p:close() end
  if not root or root == '' then root = path end
  root_cache[path] = root
  return root
end

-- Must match M.pipe_name in socket-dispatcher/init.lua exactly. Named pipes
-- are one global namespace, so the name comes from the full root path; using
-- the basename made ~/work/app and ~/project/app collide.
local function pipe_name(root)
  local s = (root:lower():gsub("[^%w]", "-"))
  if #s > 100 then s = s:sub(-100) end
  return [[\\.\pipe\nvim-]] .. s
end

local defaults = {
  nvim_bin    = "nvim",
  sock_path   = "/.nvim.sock",
  extensions  = "cpp|cc|cxx|hpp|h|c|cs|rs|py|lua",
  focus_nvim  = false,
  debug       = false,
}

function M.apply_to_config(config, opts)
  opts = opts or {}
  for k, v in pairs(defaults) do
    if opts[k] == nil then opts[k] = v end
  end

  config.hyperlink_rules = config.hyperlink_rules or wezterm.default_hyperlink_rules()

  -- compilers, linters, grep:  file.ext:12:5  or  file.ext(12,5)
  table.insert(config.hyperlink_rules, {
    regex = [[([\w./+-]+\.(?:]] .. opts.extensions .. [[))[:(](\d+)[:,](\d+)]],
    format = "nvimjump://$1:$2:$3",
  })

  -- python tracebacks:  File "path.py", line 12
  table.insert(config.hyperlink_rules, {
    regex = [[File "([^"]+\.py)", line (\d+)]],
    format = "nvimjump://$1:$2:1",
  })

  wezterm.on("open-uri", function(window, pane, uri)
    local file, line, col = uri:match("^nvimjump://(.+):(%d+):(%d+)$")
    if not file then return true end

    local cwd = pane:get_current_working_dir()
    if not cwd then return true end

    local root = git_root(cwd.file_path)
    local sock = is_windows and pipe_name(root) or (root .. opts.sock_path)

    -- absolute is "/..." on unix but "C:\..." or "C:/..." on windows
    local absolute = file:match("^/") or file:match("^%a:[/\\]")
    if not absolute then
      file = cwd.file_path .. "/" .. file
    end

    if opts.debug then
      wezterm.log_info("dispatcher: sock=" .. sock .. " file=" .. file)
    end

    wezterm.background_child_process({
      opts.nvim_bin, "--server", sock, "--remote-send",
      ([[<C-\><C-N>:lua Jump("%s",%s,%s)<CR>]]):format(file, line, col),
    })

    if opts.focus_nvim then
      for _, p in ipairs(pane:tab():panes()) do
        if (p:get_foreground_process_name() or ""):find("nvim") then
          p:activate()
          break
        end
      end
    end

    return false
  end)
end

return M
