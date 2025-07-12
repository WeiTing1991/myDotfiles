local M = {}

function M.with_custom_select(fn)
  require("fzf-lua").register_ui_select({
    winopts = {
      height = 0.5,
      width = 0.9,
      preview = {
        layout = "vertical",
        vertical = "up:40%",
        hidden = true,
      },
    },
  })

  fn()

  vim.schedule(function()
    require("fzf-lua").deregister_ui_select()
  end)
end

return M
