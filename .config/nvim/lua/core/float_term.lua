-- stole from https://github.com/MariaSolOs/dotfiles/blob/main/.config/nvim/lua/float_term.lua
local M = {}

---@type LazyFloat?
local terminal = nil

--- Opens an interactive floating terminal.
---@param cmd? string
---@param opts? LazyCmdOptions
function M.float_term(cmd, opts)
  local is_windows = vim.loop.os_uname().sysname == "Windows_NT"
  local is_mac = vim.loop.os_uname().sysname == "Darwin"
  local is_linux = vim.loop.os_uname().sysname == "linux"

  opts = vim.tbl_deep_extend("force", {
    ft = "lazyterm",
    size = { width = 0.7, height = 0.7 },
    persistent = true,
  }, opts or {})

  if is_windows then
    vim.opt.shell = "pwsh.exe"
    vim.opt.shellcmdflag = "-NoLogo -Command"
  elseif is_mac then
    vim.opt.shell = "/bin/zsh"
    vim.opt.shellcmdflag = "-c"
  end

  if terminal and terminal:buf_valid() and vim.b[terminal.buf].lazyterm_cmd == cmd then
    terminal:toggle()
  else
    terminal = require("lazy.util").float_term(cmd, opts)
    vim.b[terminal.buf].lazyterm_cmd = cmd
  end
end

return M
