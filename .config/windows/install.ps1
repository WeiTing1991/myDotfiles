# Set the PowerShell execution policy to allow running scripts
# powershell 7
winget install --id Microsoft.PowerShell --source winget

Get-ExecutionPolicy
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser

# scoop
Invoke-RestMethod -Uri https://get.scoop.sh | Invoke-Expression

# [Environment]::SetEnvironmentVariable("HOME", "C:\Users\weichen34", "User")

# AutoHotkey script
# $env:LOCALAPPDATA -> \AppData\Local\
# $env:APPDATA -> \AppData\Roaming\
winget install -e --id AutoHotkey.AutoHotkey
$startupFolderPath = "$HOME\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Startup\windos_11_hotkeys.ahk"
$dotfilesAHKPath = "$HOME\.dotfiles\.config\windows\windos_11_hotkeys.ahk"
New-Item -Path $startupFolderPath -ItemType SymbolicLink -Value $dotfilesAHKPath -Force

# Terminal
winget install -e --id raphamorim.rio
$startupFolderPath = "$HOME\AppData\Local\rio"
$dotfilesAHKPath = "$HOME\.dotfiles\.config\rio"
New-Item -Path $startupFolderPath -ItemType SymbolicLink -Value $dotfilesAHKPath -Force

$startupFolderPath = "$HOME\AppData\Local\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState\settings.json"
$dotfilesAHKPath = "$HOME\.dotfiles\.config\windows\term_settings.json"
New-Item -Path $startupFolderPath -ItemType SymbolicLink -Value $dotfilesAHKPath -Force

# tool
winget install --id=liule.Snipaste  -e

# lazygit
scoop bucket add extras
scoop install lazygit
$profileScriptPath = "$env:APPDATA\lazygit"
$dotfilesScriptPath = "$env:HOME\.dotfiles\.config\lazygit"
New-Item -Path $profileScriptPath -ItemType SymbolicLink -Value $dotfilesScriptPath -Force

# dev tool
scoop install fd ripgrep make cmake wget unzip
scoop install neovim
scoop install mingw

# scoop install fzf bat gzip

# NVIM
rm -Force ~\AppData\Local\nvim
rm -Force ~\AppData\Local\nvim-data

$profileScriptPath = "$env:LOCALAPPDATA\nvim"
$dotfilesScriptPath = "$env:HOME\.dotfiles\.config\nvim"
New-Item -Path $profileScriptPath -ItemType SymbolicLink -Value $dotfilesScriptPath -Force

# zed
# New-Item -Path $HOME\AppData\Roaming\Zed -ItemType SymbolicLink -Value $HOME\.dotfiles\.config\zed -Force
