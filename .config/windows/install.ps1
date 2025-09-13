# Set the PowerShell execution policy to allow running scripts
Get-ExecutionPolicy
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser

# powershell 7
winget install --id Microsoft.PowerShell --source winget
Install-Module -Name PowerShellGet -RequiredVersion 2.2.5 -Force

# scoop
Invoke-RestMethod -Uri https://get.scoop.sh | Invoke-Expression

# [Environment]::SetEnvironmentVariable("HOME", "C:\Users\weichen34", "User")
ls $env:LOCALAPPDATA
ls $env:HOME

# AutoHotkey script
# $env:LOCALAPPDATA -> \AppData\Local\
# $env:APPDATA -> \AppData\Roaming\
winget install -e --id AutoHotkey.AutoHotkey
$startupFolderPath = "$HOME\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Startup\windos_11_hotkeys.ahk"
$dotfilesAHKPath = "$HOME\.dotfiles\.config\windows\windos_11_hotkeys.ahk"
New-Item -Path $startupFolderPath -ItemType SymbolicLink -Value $dotfilesAHKPath -Force

# Terminal
$startupFolderPath = "$HOME\AppData\Local\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState\settings.json"
$dotfilesAHKPath = "$HOME\.dotfiles\.config\windows\term_settings.json"
New-Item -Path $startupFolderPath -ItemType SymbolicLink -Value $dotfilesAHKPath -Force

$startupFolderPath = "$HOME\.config\wezterm"
$dotfilesAHKPath = "$HOME\.dotfiles\.config\wezterm"
New-Item -Path $startupFolderPath -ItemType SymbolicLink -Value $dotfilesAHKPath -Force

# tool
winget install --id=liule.Snipaste  -e
scoop bucket add extras
scoop bucket add version

# lazygit
scoop install lazygit
$profileScriptPath = "$env:APPDATA\lazygit"
$dotfilesScriptPath = "$env:HOME\.dotfiles\.config\lazygit"
New-Item -Path $profileScriptPath -ItemType SymbolicLink -Value $dotfilesScriptPath -Force

# Dev tool
scoop install fd ripgrep make cmake wget unzip gzip
scoop install neovim
scoop install mingw
scoop install tree-sitter

# scoop install fzf bat gzip

# NVIM
rm -Force ~\AppData\Local\nvim
rm -Force ~\AppData\Local\nvim-data

$profileScriptPath = "$env:LOCALAPPDATA\nvim"
$dotfilesScriptPath = "$env:HOME\.dotfiles\.config\nvim"
New-Item -Path $profileScriptPath -ItemType SymbolicLink -Value $dotfilesScriptPath -Force

# zed
# https://zed.dev/docs/development/windows#building-zed-for-windows
$profileScriptPath = "$env:APPDATA\Zed"
$dotfilesScriptPath = "$env:HOME\.dotfiles\.config\zed"
New-Item -Path $profileScriptPath -ItemType SymbolicLink -Value $dotfilesScriptPath -Force

$profileScriptPath = "$env:APPDATA\Sublime Text\Packages\User"
$dotfilesScriptPath = "$env:HOME\.dotfiles\.config\sublime\User"
New-Item -Path $profileScriptPath -ItemType SymbolicLink -Value $dotfilesScriptPath -Force
