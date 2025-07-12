local vscode = require("vscode")

vim.keymap.set("n", "<leader>ff", function()
  vscode.action("workbench.action.quickOpen")
end)

vim.keymap.set("n", "<leader>fg", function()
  vscode.action("workbench.action.quickTextSearch")
end)

vim.keymap.set("n", "g.", function()
  vscode.action("editor.action.codeAction")
end)

vim.keymap.set("n", "<leader>,", function()
  vscode.action("editor.action.formatDocument.multiple")
end)

vim.keymap.set({"n","v",}, "-", function()
  vscode.action("editor.action.commentLine")
end)

vim.keymap.set({"n","v",}, "_", function()
  vscode.action("editor.action.blockComment")
end)

vim.keymap.set({"v"}, "J", function()
  vscode.action("editor.action.moveLinesDownAction")
end)

vim.keymap.set({"v"}, "K", function()
  vscode.action("editor.action.moveLinesUpAction")
end)
