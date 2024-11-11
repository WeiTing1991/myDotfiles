# install neovim and wezterm
winget install neovim
winget install wez.wezterm
winget install --id Microsoft.Powershell --source winget

# for telescope (fzf)
# winget install BurntSushi.ripgrep.MSVC
# winget install sharkdp.fd
# winget install --id=junegunn.fzf  -e

Remove-Item $env:USERPROFILE\AppData\Local\nvim -Recurse -Force
Remove-Item $env:USERPROFILE\AppData\Local\nvim-data -Recurse -Force

# link config file to user dir
# neovim and wezterm
New-Item -Path $env:USERPROFILE\AppData\Local\nvim\ -ItemType SymbolicLink -Value $env:USERPROFILE\.dotfiles\.config\nvim\ -Force
New-Item -Path "C:\Program Files\WezTerm\wezterm.lua" -ItemType SymbolicLink -Value $env:USERPROFILE\.mydotfiles\.config\wezterm_win\wezterm.lua -Force

# emacs
New-Item -Path $env:USERPROFILE\.emacs.d\ -ItemType SymbolicLink -Value $env:USERPROFILE\.dotfiles\.emacs.d\ -Force

# zed
New-Item -Path $env:USERPROFILE\AppData\Roaming\Zed\ -ItemType SymbolicLink -Value $env:USERPROFILE\.dotfiles\.config\zed\ -Force

# window tilling
New-Item -Path $env:USERPROFILE\komorebi.bar.json -ItemType SymbolicLink -Value $env:USERPROFILE\.dotfiles\.config\komorebi\komorebi.bar.json -Force
New-Item -Path $env:USERPROFILE\komorebi.json -ItemType SymbolicLink -Value $env:USERPROFILE\.dotfiles\.config\komorebi\komorebi.json -Force
New-Item -Path $env:USERPROFILE\.config\whkdrc -ItemType SymbolicLink -Value $env:USERPROFILE\.dotfiles\.config\whkdrc -Force

#komorebic stop --whkd --bar

# Powershell
New-Item -Path $env:USERPROFILE\.config\powershell\weitingchen.ps1 -ItemType SymbolicLink -Value $env:USERPROFILE\.dotfiles\.config\powershell\weitingchen.ps1 -Force

# New-Item -Path $env:USERPROFILE\.vim\.vimrc -ItemType SymbolicLink -Value $env:USERPROFILE\myDotfiles\.vimrc -Force


# vscode

New-Item -Path $env:USERPROFILE\AppData\Roaming\Code\User\settings.json -ItemType SymbolicLink -Value $env:USERPROFILE\.dotfiles\.vscode\settings.json -Force
New-Item -Path $env:USERPROFILE\AppData\Roaming\Code\User\keybindings.json -ItemType SymbolicLink -Value $env:USERPROFILE\.dotfiles\.vscode\keybindings.json -Force

