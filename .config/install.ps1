# Set the PowerShell execution policy to allow running scripts
Get-ExecutionPolicy
Set-ExecutionPolicy RemoteSigned -Scope CurrentUser

# Add the script to the PowerShell profile
$profileScriptPath = "$HOME\.config\powershell\weitingchen.ps1"
$dotfilesScriptPath = "$HOME\.dotfiles\.config\windows\powershell\weitingchen.ps1"
New-Item -Path $profileScriptPath -ItemType SymbolicLink -Value $dotfilesScriptPath -Force

# Create symbolic link for Alacritty configuration
$alacrittyConfigPath = "$APPDATA\alacritty\alacritty.toml"
$dotfilesAlacrittyPath = "$HOME\.dotfiles\.config\alacritty\alacritty.toml"
New-Item -Path $alacrittyConfigPath -ItemType SymbolicLink -Value $dotfilesAlacrittyPath -Force

# Create symbolic link for AutoHotkey script
$startupFolderPath = "$APPDATA\Microsoft\Windows\Start Menu\Programs\Startup\windos_11_hotkeys.ahk"
$dotfilesAHKPath = "$HOME\.dotfiles\.config\windows\windos_11_hotkeys.ahk"
New-Item -Path $startupFolderPath -ItemType SymbolicLink -Value $dotfilesAHKPath -Force

# Uncomment the following line to create a symbolic link for Vim configuration (if needed)
# $vimrcLinkPath = "$HOME\.vim\.vimrc"
# $dotfilesVimrcPath = "$HOME\myDotfiles\.vimrc"
# New-Item -Path $vimrcLinkPath -ItemType SymbolicLink -Value $dotfilesVimrcPath -Force
