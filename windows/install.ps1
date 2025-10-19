# Set the PowerShell execution policy to allow running scripts
Get-ExecutionPolicy
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser

# powershell 7
winget install --id Microsoft.PowerShell --source winget
Install-Module -Name PowerShellGet -RequiredVersion 2.2.5 -Force

# scoop
if (Test-Path "$env:USERPROFILE\scoop") {
    Write-Host "Scoop is already installed."
} else {
    Write-Host "Installing Scoop..."
    Invoke-RestMethod -Uri https://get.scoop.sh | Invoke-Expression
}


# [Environment]::SetEnvironmentVariable("HOME", "C:\Users\weichen34", "User")
ls $env:LOCALAPPDATA
ls $env:HOME

. "$PSScriptRoot\helpers.ps1"

# $env:LOCALAPPDATA -> \AppData\Local\
# $env:APPDATA -> \AppData\Roaming\

# AutoHotkey script
Install-ProgramAndLinkDotfiles `
-program "AutoHotkey.AutoHotkey" `
-dotfilesPath "windows\windos_11_hotkeys.ahk" `
-targetPath "$env:APPDATA\Microsoft\Windows\Start Menu\Programs\Startup\windos_11_hotkeys.ahk"

# Warp
Install-ProgramAndLinkDotfiles `
-program "Warp.Warp" `
-dotfilesPath ".warp\keybindings.yaml" `
-targetPath "$env:LOCALAPPDATA\warp\Warp\config\keybindings.yaml"

# Windows Terminal
Install-ProgramAndLinkDotfiles `
    -program "" `
    -dotfilesPath "windows\term_settings.json" `
    -targetPath "$env:LOCALAPPDATA\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState\settings.json"

# WezTerm (linking entire directory)
Install-ProgramAndLinkDotfiles `
    -program "" `
    -dotfilesPath ".config\wezterm" `
    -targetPath "$HOME\.config\wezterm"

# Starship
Install-ProgramAndLinkDotfiles `
    -program "Starship.Starship" `
    -dotfilesPath ".config\starship.toml" `
    -targetPath "$HOME\.config\starship.toml"


# Tool
winget install --id=liule.Snipaste  -e
scoop bucket add extras
scoop bucket add version

