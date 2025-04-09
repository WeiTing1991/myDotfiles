if vim.g.neovide then
  vim.keymap.set("n", "<D-s>", ":w<CR>")      -- Save
  vim.keymap.set("v", "<D-c>", '"+y')         -- Copy
  vim.keymap.set("n", "<D-v>", '"+P')         -- Paste normal mode
  vim.keymap.set("v", "<D-v>", '"+P')         -- Paste visual mode
  vim.keymap.set("c", "<D-v>", "<C-R>+")      -- Paste command mode
  vim.keymap.set("i", "<D-v>", '<ESC>l"+Pli') -- Paste insert mode

  -- only in neovide
  vim.keymap.set("n", "<leader>o", ":bnext<cr>", { desc = "Next buffer" })
  vim.keymap.set("n", "<leader>i", ":bprevious<cr>", { desc = "Prevous buffer" })

  vim.g.neovide_cursor_animation_length = 0
  vim.g.neovide_cursor_antialiasing = true
  vim.g.neovide_cursor_animate_in_insert_mode = true
  vim.g.neovide_cursor_trail_length = 0
  vim.g.neovide_cursor_vfx_mode = ""
  -- vim.neovide_cursor_vfx_opacity = 0.0
  -- vim.neovide_cursor_vfx_particle_density = 0.0
  vim.opt.guicursor = "i:ver25-Cursor"
end
require "core"
