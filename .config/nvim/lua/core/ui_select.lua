local M = {}

function M.ui_select(fn)
  fn(require("telescope.themes").get_dropdown({
    enable_preview = true,
    winblend = 10,
  }))
  -- vim.schedule(function()
  --   require("fzf-lua").deregister_ui_select()
  -- end)
end

return M
