local M = {}

M.is_windows = vim.uv.os_uname().sysname == "Windows_NT"
M.is_mac = vim.uv.os_uname().sysname == "Darwin"

local saved_dims = {}

function M.toggle_maximize_window()
  local win = vim.api.nvim_get_current_win()
  if saved_dims[win] then
    vim.api.nvim_win_set_width(win, saved_dims[win].width)
    vim.api.nvim_win_set_height(win, saved_dims[win].height)
    saved_dims[win] = nil
  else
    saved_dims[win] = {
      width = vim.api.nvim_win_get_width(win),
      height = vim.api.nvim_win_get_height(win),
    }
    vim.cmd("resize | vertical resize")
  end
end

return M
