require("trouble").setup {}
vim.keymap.set({ "n", "v" }, "<leader>t", "<cmd>Trouble diagnostics toggle<cr>", { desc = "toggle trouble" })

