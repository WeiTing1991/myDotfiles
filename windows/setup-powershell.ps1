$profileScriptPath = "$HOME\Documents\PowerShell\Profile.ps1"
$dotfilesScriptPath = "$HOME\.dotfiles\windows\Profile.ps1"
New-Item -Path $profileScriptPath -ItemType SymbolicLink -Value $dotfilesScriptPath -Force

PowerShellGet\Install-Module posh-git -Scope CurrentUser -Force
PowerShellGet\Install-Module PSReadLine -Scope CurrentUser -Force
