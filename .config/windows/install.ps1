# Set the PowerShell execution policy to allow running scripts
# powershell 7
winget install --id Microsoft.PowerShell --source winget

Get-ExecutionPolicy
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser

# scoop
Invoke-RestMethod -Uri https://get.scoop.sh | Invoke-Expression

# [Environment]::SetEnvironmentVariable("HOME", "C:\Users\weichen34", "User")

# Create symbolic link for AutoHotkey script
winget install -e --id AutoHotkey.AutoHotkey
$startupFolderPath = "$HOME\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Startup\windos_11_hotkeys.ahk"
$dotfilesAHKPath = "$HOME\.dotfiles\.config\windows\windos_11_hotkeys.ahk"
New-Item -Path $startupFolderPath -ItemType SymbolicLink -Value $dotfilesAHKPath -Force

winget install -e --id raphamorim.rio
$startupFolderPath = "$HOME\AppData\Local\rio"
$dotfilesAHKPath = "$HOME\.dotfiles\.config\rio"
New-Item -Path $startupFolderPath -ItemType SymbolicLink -Value $dotfilesAHKPath -Force

# $startupFolderPath = "$HOME\AppData\Local\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState\settings.json"
# $dotfilesAHKPath = "$HOME\.dotfiles\.config\windows\term_settings.json"
# New-Item -Path $startupFolderPath -ItemType SymbolicLink -Value $dotfilesAHKPath -Force

# tool
winget install --id=liule.Snipaste  -e

# dev tool
# zig ass gcc
scoop install main/gcc

# lazygit
scoop bucket add extras
scoop install lazygit
$profileScriptPath = "$HOME\AppData\Roaming\lazygit\config"
$dotfilesScriptPath = "$HOME\.dotfiles\.config\lazygit"
New-Item -Path $profileScriptPath -ItemType SymbolicLink -Value $dotfilesScriptPath -Force

# NVIM
scoop install fd fzf ripgrep bat make wget unzip gzip
rm -Force ~\AppData\Local\nvim
rm -Force ~\AppData\Local\nvim-data

$profileScriptPath = "$HOME\AppData\Local\nvim"
$dotfilesScriptPath = "$HOME\.dotfiles\.config\nvim_win\nvim"
New-Item -Path $profileScriptPath -ItemType SymbolicLink -Value $dotfilesScriptPath -Force

# zed
# New-Item -Path $HOME\AppData\Roaming\Zed -ItemType SymbolicLink -Value $HOME\.dotfiles\.config\zed -Force
