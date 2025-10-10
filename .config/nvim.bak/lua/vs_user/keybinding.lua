local vscode = require("vscode")

vim.keymap.set("n", "<leader>ff", function()
  vscode.action("workbench.action.quickOpen")
end)

vim.keymap.set("n", "<leader>fl", function()
  vscode.action("workbench.action.quickTextSearch")
end)

vim.keymap.set("n", "<leader>fg", function()
  vscode.action("television.ToggleTextFinder")
end)

vim.keymap.set("n", "g.", function()
  vscode.action("editor.action.codeAction")
end)

vim.keymap.set("n", "<leader>,", function()
  vscode.action("editor.action.formatDocument.multiple")
end)

vim.keymap.set({ "n", "v" }, "-", function()
  vscode.action("editor.action.commentLine")
end)

vim.keymap.set({ "n", "v" }, "_", function()
  vscode.action("editor.action.blockComment")
end)

vim.keymap.set({ "v" }, "J", function()
  vscode.action("editor.action.moveLinesDownAction")
end)

vim.keymap.set({ "v" }, "K", function()
  vscode.action("editor.action.moveLinesUpAction")
end)

vim.keymap.set("n", "<leader>d", function()
  -- vscode.action("projectManager.refreshGitProjects")
  vscode.action("projectManager.listProjectsNewWindow")
end)

vim.keymap.set("n", "gi", function()
  vscode.action("editor.action.goToImplementation")
end)

vim.keymap.set("n", "gr", function()
  vscode.action("editor.action.goToReferences")
end)
