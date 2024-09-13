require("trouble").setup {}

-- Keybindings
vim.keymap.set({ "n", "v" }, "<leader>t", "<cmd>Trouble diagnostics toggle<cr>", { desc = "toggle trouble" })

vim.keymap.set("n", "t[", function()
  require("trouble").next { skip_groups = true, jump = true }
end)

vim.keymap.set("n", "t]", function()
  require("trouble").previous { skip_groups = true, jump = true }
end)
