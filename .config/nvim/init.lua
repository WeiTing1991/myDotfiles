if vim.g.vscode then
  require("vs_user.settings")
  print("VSCode detected, loading VSCode settings")
else
  vim.cmd.colorscheme "bella-gruvbox"
  -- vim.cmd.colorscheme("habamax")
  require("core")
end
