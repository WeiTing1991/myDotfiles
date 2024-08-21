
# install neovim and wezterm
winget install neovim
winget install wez.wezterm

Remove-Item $env:USERPROFILE\AppData\Local\nvim -Recurse -Force
Remove-Item $env:USERPROFILE\AppData\Local\nvim-data -Recurse -Force

# link config file to user dir
New-Item -Path $env:USERPROFILE\AppData\Local\nvim\ -ItemType SymbolicLink -Value $env:USERPROFILE\myDotfiles\.config\nvim\ -Force
New-Item -Path "C:\Program Files\WezTerm\wezterm.lua" -ItemType SymbolicLink -Value $env:USERPROFILE\.mydotfiles\.config\wezterm_win\wezterm.lua -Force

# New-Item -Path $env:USERPROFILE\.vim\.vimrc -ItemType SymbolicLink -Value $env:USERPROFILE\myDotfiles\.vimrc -Force
