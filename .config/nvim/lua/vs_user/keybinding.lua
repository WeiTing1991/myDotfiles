local vscode = require("vscode")

vim.keymap.set("n", "<leader>ff", function()
  vscode.action("workbench.action.quickOpen")
end)

vim.keymap.set("n", "g.", function()
  vscode.action("editor.action.codeAction")
end)

vim.keymap.set("n", "<leader>,", function()
  vscode.action("editor.action.formatDocument.multiple")
end)
