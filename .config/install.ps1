# Set the PowerShell execution policy to allow running scripts
winget install --id Microsoft.PowerShell --source winget

Get-ExecutionPolicy
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser

# scoop
Invoke-RestMethod -Uri https://get.scoop.sh | Invoke-Expression

# Create symbolic link for AutoHotkey script
$startupFolderPath = "$HOME\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Startup\windos_11_hotkeys.ahk"
$dotfilesAHKPath = "$HOME\.dotfiles\.config\windows\windos_11_hotkeys.ahk"
New-Item -Path $startupFolderPath -ItemType SymbolicLink -Value $dotfilesAHKPath -Force

# Add `$HOME` into path
# Add the script to the PowerShell profile
$profileScriptPath = "$HOME\.config\powershell\weitingchen.ps1"
$dotfilesScriptPath = "$HOME\.dotfiles\.config\windows\weitingchen.ps1"
New-Item -Path $profileScriptPath -ItemType SymbolicLink -Value $dotfilesScriptPath -Force

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
