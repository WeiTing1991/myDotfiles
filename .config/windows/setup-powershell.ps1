$profileScriptPath = "$HOME\Documents\PowerShell\Profile.ps1"
$dotfilesScriptPath = "$HOME\.dotfiles\.config\windows\Profile.ps1"
New-Item -Path $profileScriptPath -ItemType SymbolicLink -Value $dotfilesScriptPath -Force