
# install neovim and wezterm
winget install neovim
winget install wez.wezterm
winget install --id Microsoft.Powershell --source winget

# for telescope (fzf)
winget install BurntSushi.ripgrep.MSVC
winget install sharkdp.fd
# winget install --id=junegunn.fzf  -e

Remove-Item $env:USERPROFILE\AppData\Local\nvim -Recurse -Force
Remove-Item $env:USERPROFILE\AppData\Local\nvim-data -Recurse -Force

# link config file to user dir
New-Item -Path $env:USERPROFILE\AppData\Local\nvim\ -ItemType SymbolicLink -Value $env:USERPROFILE\.dotfiles\.config\nvim\ -Force
New-Item -Path $env:USERPROFILE\AppData\Roaming\Zed\ -ItemType SymbolicLink -Value $env:USERPROFILE\.dotfiles\.config\zed\ -Force
New-Item -Path "C:\Program Files\WezTerm\wezterm.lua" -ItemType SymbolicLink -Value $env:USERPROFILE\.mydotfiles\.config\wezterm_win\wezterm.lua -Force
#New-Item -Path $env:USERPROFILE\.emacs.d\ -ItemType SymbolicLink -Value $env:USERPROFILE\.dotfiles\.emacs.d\ -Force
#New-Item -Path $env:USERPROFILE\.doom.d\ -ItemType SymbolicLink -Value $env:USERPROFILE\.dotfiles\.doom.d\ -Force

# New-Item -Path $env:USERPROFILE\.vim\.vimrc -ItemType SymbolicLink -Value $env:USERPROFILE\myDotfiles\.vimrc -Force
