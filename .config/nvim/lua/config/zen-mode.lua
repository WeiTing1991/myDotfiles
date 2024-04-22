local zen_mode = require "zen-mode"

vim.keymap.set("n", "<leader>zz", function()
  zen_mode.setup {
    window = {
      width = .90,
      options = {
      },
    },
    weztem ={
    enabled = true,
    }
  }
    zen_mode.toggle()
end)
