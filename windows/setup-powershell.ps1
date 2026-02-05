$profileScriptPath = "$HOME\Documents\PowerShell\Profile.ps1"
$dotfilesScriptPath = "$HOME\.dotfiles\windows\Profile.ps1"
New-Item -Path $profileScriptPath -ItemType SymbolicLink -Value $dotfilesScriptPath -Force

PowerShellGet\Install-Module posh-git -Scope CurrentUser -Force
PowerShellGet\Install-Module PSReadLine -Scope CurrentUser -Force

$starshipConfigPath = "$HOME\.config\starship.toml"
$dotfilesStarshipPath = "$HOME\.dotfiles\.config\starship.toml"
New-Item -Path (Split-Path $starshipConfigPath) -ItemType Directory -Force -ErrorAction SilentlyContinue
New-Item -Path $starshipConfigPath -ItemType SymbolicLink -Value $dotfilesStarshipPath -Force
