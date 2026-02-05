. "$PSScriptRoot\helpers.ps1"
# scoop bucket add extras
# scoop bucket add versions

# Dev tool
scoop install fd ripgrep make cmake wget unzip gzip fzf
scoop install wezterm-nightly
scoop install neovim lazygit
scoop install diff-so-fancy
scoop install mingw
scoop install bat
scoop install eza

# scoop install tree-sitter
# scoop install bat

# lazygit
LinkDotfiles `
-program "" `
-dotfilesPath ".config\lazygit" `
-targetPath "$env:APPDATA\lazygit"

# nvm/nodejs
# scoop install nvm
# nvm install node
# nvm use newest

# NVIM
rm -Force ~\AppData\Local\nvim
rm -Force ~\AppData\Local\nvim-data
Install-ProgramAndLinkDotfiles `
-dotfilesPath ".config\nvim" `
-targetPath "$env:LOCALAPPDATA\nvim"

# VS
LinkDotfiles `
-program "" `
-dotfilesPath "windows\.vsvimrc" `
-targetPath "$HOME\.vsvimrc"

# uv
powershell -ExecutionPolicy ByPass -c "irm https://astral.sh/uv/install.ps1 | iex"
$env:Path = "$HOME\.local\bin;$env:Path"
uv --version
add python
$env:PATH = "$HOME\AppData\Roaming\uv\python;" + $env:PATH

scoop install mamba
powershell -ExecutionPolicy ByPass -c "irm -useb https://pixi.sh/install.ps1 | iex"


# UV Python configuration
# Install-ProgramAndLinkDotfiles `
# -program "" `
# -dotfilesPath ".config\uv\uv.toml" `
# -targetPath "$HOME\.config\uv\uv.toml"

# Pip configuration
# Install-ProgramAndLinkDotfiles `
#     -program "" `
#     -dotfilesPath ".config\pip\pip.conf" `
#     -targetPath "$env:APPDATA\pip\pip.ini

# docker
# winget install -e --id Docker.DockerDesktop

# dotnet
# winget search Microsoft.DotNet
# winget install Microsoft.DotNet.SDK.9

# bun
# powershell -c "irm bun.sh/install.ps1 | iex"

# check rustup
# $rust_link = "https://www.rust-lang.org/tools/install"
# Writ-Host "rust: $rust_link"

# fzf tool (test)
# $startupFolderPath = "$env:LOCALAPPDATA\television"
# $dotfilesAHKPath = "$HOME\.dotfiles\.config\television"
# New-Item -Path $startupFolderPath -ItemType SymbolicLink -Value $dotfilesAHKPath -Force

# # Bun configuration
# Install-ProgramAndLinkDotfiles `
#     -program "" `
#     -dotfilesPath ".config\bun\bunfig.toml" `
#     -targetPath "$HOME\.bunfig.toml"

# # Cargo (Rust) configuration
# Install-ProgramAndLinkDotfiles `
#     -program "" `
#     -dotfilesPath ".config\cargo\config.toml" `
#     -targetPath "$HOME\.cargo\config.toml"
#
# NEED TO MOVED
# zed
# # https://zed.dev/docs/development/windows#building-zed-for-windows
# $profileScriptPath = "$env:APPDATA\Zed"
# $dotfilesScriptPath = "$env:HOME\.dotfiles\.config\zed"
# New-Item -Path $profileScriptPath -ItemType SymbolicLink -Value $dotfilesScriptPath -Force
# $profileScriptPath = "$env:APPDATA\Sublime Text\Packages\User"
# $dotfilesScriptPath = "$env:HOME\.dotfiles\.config\sublime\User"
