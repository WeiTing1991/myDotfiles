# Optional: Remove .git folder inside nvim folder

Remove-Item $env:LOCALAPPDATA\nvim -Recurse -Force
Remove-Item $env:LOCALAPPDATA\nvim-data -Recurse -Force

# link config file to user dir
New-Item -Path ~\AppData\Local\nvim -Value ~\myDotfiles\.config\nvim -ItemType SymbolicLink -Forcer
New-Item -Path "C:\Program Files\WezTerm\wezterm.lua" -ItemType SymbolicLink -Value ~\myDotfiles\.config\wezterm\wezterm.lua -Force
