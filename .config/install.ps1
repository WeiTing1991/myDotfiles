# Set the PowerShell execution policy to allow running scripts
Get-ExecutionPolicy
Set-ExecutionPolicy RemoteSigned -Scope CurrentUser

winget install Neovim.Neovim

# # scoop
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser
Invoke-RestMethod -Uri https://get.scoop.sh | Invoke-Expression

# zig ass gcc
scoop install main/gcc

# Add `$HOME` into path
# Add the script to the PowerShell profile
$profileScriptPath = "$HOME\.config\powershell\weitingchen.ps1"
$dotfilesScriptPath = "$HOME\.dotfiles\.config\windows\weitingchen.ps1"
New-Item -Path $profileScriptPath -ItemType SymbolicLink -Value $dotfilesScriptPath -Force

# yazi
# scoop install yazi
winget install sxyazi.yazi
[Environment]::SetEnvironmentVariable("YAZI_FILE_ONE", "C:\Program Files\Git\usr\bin\file.exe", "User")
# keybing not working
$profileScriptPath = "$HOME\AppData\Roaming\yazi\config"
$dotfilesScriptPath = "$HOME\.dotfiles\.config\yazi"
New-Item -Path $profileScriptPath -ItemType SymbolicLink -Value $dotfilesScriptPath -Force

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
$dotfilesScriptPath = "$HOME\.dotfiles\.config\nvim"
New-Item -Path $profileScriptPath -ItemType SymbolicLink -Value $dotfilesScriptPath -Force

# Create symbolic link for AutoHotkey script
# $startupFolderPath = "$APPDATA\Microsoft\Windows\Start Menu\Programs\Startup\windos_11_hotkeys.ahk"
# $dotfilesAHKPath = "$HOME\.dotfiles\.config\windows\windos_11_hotkeys.ahk"
# New-Item -Path $startupFolderPath -ItemType SymbolicLink -Value $dotfilesAHKPath -Force


# zed
New-Item -Path $HOME\AppData\Roaming\Zed -ItemType SymbolicLink -Value $HOME\.dotfiles\.config\zed -Force
