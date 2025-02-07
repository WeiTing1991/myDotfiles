# install neovim and wezterm
winget install --id Microsoft.Powershell --source winget
winget install neovim
winget install wez.wezterm

# for telescope (fzf)
# winget install BurntSushi.ripgrep.MSVC
# winget install sharkdp.fd
# winget install --id=junegunn.fzf  -e

# link config file to user dir

# Powershell
New-Item -Path $env:USERPROFILE\.config\powershell\weitingchen.ps1 -ItemType SymbolicLink -Value $env:USERPROFILE\.dotfiles\.config\powershell\weitingchen.ps1 -Force

# Weterm
New-Item -Path "C:\Program Files\WezTerm\wezterm.lua" -ItemType SymbolicLink -Value $env:USERPROFILE\.dotfiles\.config\wezterm\wezterm.lua -Force

# neovim and wezterm
Remove-Item $env:USERPROFILE\AppData\Local\nvim -Recurse -Force
Remove-Item $env:USERPROFILE\AppData\Local\nvim-data -Recurse -Force

New-Item -Path $env:USERPROFILE\AppData\Local\nvim\ -ItemType SymbolicLink -Value $env:USERPROFILE\.dotfiles\.config\nvim\ -Force

# zed
New-Item -Path $env:USERPROFILE\AppData\Roaming\Zed\ -ItemType SymbolicLink -Value $env:USERPROFILE\.dotfiles\.config\zed\ -Force

# New-Item -Path $env:USERPROFILE\.vim\.vimrc -ItemType SymbolicLink -Value $env:USERPROFILE\myDotfiles\.vimrc -Force

# vscode
New-Item -Path $env:USERPROFILE\AppData\Roaming\Code\User\settings.json -ItemType SymbolicLink -Value $env:USERPROFILE\.dotfiles\.vscode\settings.json -Force
New-Item -Path $env:USERPROFILE\AppData\Roaming\Code\User\keybindings.json -ItemType SymbolicLink -Value $env:USERPROFILE\.dotfiles\.vscode\keybindings.json -Force

