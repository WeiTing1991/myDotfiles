. "$PSScriptRoot\helpers.ps1"

# Windows Terminal
Install-ProgramAndLinkDotfiles `
    -program "" `
    -dotfilesPath "windows\term_settings.json" `
    -targetPath "$env:LOCALAPPDATA\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState\settings.json"
