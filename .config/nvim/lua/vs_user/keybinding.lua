local vscode = require("vscode")

vim.keymap.set("n", "<leader>ff", function()
  vscode.action("workbench.action.quickOpen")
end)
