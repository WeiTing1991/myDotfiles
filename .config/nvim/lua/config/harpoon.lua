local harpoon = require "harpoon"

-- REQUIRED
harpoon:setup()
-- REQUIRED

vim.keymap.set("n", "<leader>aa", function()
  harpoon:list():append()
end, { desc = "harpoon append" })
vim.keymap.set("n", "<leader>a", function()
  harpoon.ui:toggle_quick_menu(harpoon:list())
end, { desc = "harpoon list" })

vim.keymap.set("n", "<leader>1", function()
  harpoon:list():select(1)
end)
vim.keymap.set("n", "<leader>2", function()
  harpoon:list():select(2)
end)
vim.keymap.set("n", "<leader>3", function()
  harpoon:list():select(3)
end)
vim.keymap.set("n", "<leader>4", function()
  harpoon:list():select(4)
end)

-- Toggle previous & next buffers stored within Harpoon list
vim.keymap.set("n", "<C-p>", function()
  harpoon:list():prev()
end, { desc = "Go to pervious mark" })
vim.keymap.set("n", "<C-n>", function()
  harpoon:list():next()
end, { desc = "Go to next mark" })
