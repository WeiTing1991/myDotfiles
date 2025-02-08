-- toggle maximize windows

local M = {}

function M.toggle_maximize_window()
  local win_count = vim.fn.winnr("$")
  local current_win = vim.api.nvim_get_current_win()

  -- Check if there is more than one window
  if win_count > 1 then
    if vim.b.maximized_window_id then
      -- Go back to the maximized window
      vim.fn.win_gotoid(vim.b.maximized_window_id)
    else
      -- Save the current window ID and maximize
      vim.b.origin_window_id = current_win
      vim.cmd("tab split")
      vim.b.maximized_window_id = vim.api.nvim_get_current_win()
    end
  else
    -- Only one window exists (maximized state)
    if vim.b.origin_window_id then
      local origin_win_id = vim.b.origin_window_id
      vim.cmd("tabclose")
      vim.fn.win_gotoid(origin_win_id)
      vim.b.maximized_window_id = nil
      vim.b.origin_window_id = nil
    end
  end
end

-- default keymaps
function M.setup()
  vim.keymap.set("n", "<c-'>", M.toggle_maximize_window, { desc = "Toggle maximize window" })
end

return M
