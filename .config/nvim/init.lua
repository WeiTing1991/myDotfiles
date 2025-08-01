if vim.g.vscode then
  require("vs_user.settings")
  print("VSCode detected, loading VSCode settings")
else
  vim.cmd.colorscheme "bella-gruvbox"
  require("core")
end
