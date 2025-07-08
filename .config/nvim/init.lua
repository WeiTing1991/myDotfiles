if vim.g.vscode then
  require "vs_user.settings"
else
  vim.cmd.colorscheme "bella-gruvbox"
  require "core"
end


if vim.g.neovide then
  -- Font settings
  vim.o.guifont = "JetBrainsMono Nerd Font:h10"  -- Set font and size

  -- Disable all animations for clean, distraction-free experience
  vim.g.neovide_cursor_animation_length = 0.0
  vim.g.neovide_cursor_trail_length = 0.0
  vim.g.neovide_scroll_animation_length = 0.0
  vim.g.neovide_cursor_vfx_mode = ""  -- No visual effects

  -- Performance settings
  vim.g.neovide_refresh_rate = 120
  vim.g.neovide_no_idle = true

  -- Window behavior
  vim.g.neovide_remember_window_size = true
  vim.g.neovide_fullscreen = false

  -- Input settings
  vim.g.neovide_hide_mouse_when_typing = true

  vim.keymap.set("n", "<C-=>", function()
    vim.g.neovide_scale_factor = vim.g.neovide_scale_factor * 1.1
  end, { desc = "Increase scale" })

  vim.keymap.set("n", "<C-->", function()
    vim.g.neovide_scale_factor = vim.g.neovide_scale_factor * 0.9
  end, { desc = "Decrease scale" })
end
