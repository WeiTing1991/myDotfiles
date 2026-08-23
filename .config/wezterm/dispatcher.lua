local wezterm = require("wezterm")

local M = {}

local root_cache = {}

local function git_root(path)
  if root_cache[path] then return root_cache[path] end
  local p = io.popen('cd ' .. ('%q'):format(path) ..
                     ' && git rev-parse --show-toplevel 2>/dev/null')
  local root = p and p:read('*l')
  if p then p:close() end
  if not root or root == '' then root = path end
  root_cache[path] = root
  return root
end

local defaults = {
  nvim_bin    = "nvim",
  sock_path   = "/.nvim/socket_dispatcher/nvim.sock",
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

    local sock = git_root(cwd.file_path) .. opts.sock_path

    if not file:match("^/") then
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
