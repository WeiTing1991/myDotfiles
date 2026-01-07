. "$PSScriptRoot\helpers.ps1"
scoop bucket add extras
scoop bucket add versions

# Dev tool
scoop install fd ripgrep make cmake wget unzip gzip fzf
scoop install neovim
scoop install laygit
scoop install mingw
#scoop install tree-sitter

# scoop install bat

# # NVIM
# rm -Force ~\AppData\Local\nvim
# rm -Force ~\AppData\Local\nvim-data
#
# $profileScriptPath = "$env:LOCALAPPDATA\nvim"
# $dotfilesScriptPath = "$env:HOME\.dotfiles\.config\nvim"
# New-Item -Path $profileScriptPath -ItemType SymbolicLink -Value $dotfilesScriptPath -Force
#
# # zed
# # https://zed.dev/docs/development/windows#building-zed-for-windows
# $profileScriptPath = "$env:APPDATA\Zed"
# $dotfilesScriptPath = "$env:HOME\.dotfiles\.config\zed"
# New-Item -Path $profileScriptPath -ItemType SymbolicLink -Value $dotfilesScriptPath -Force
#
# $profileScriptPath = "$env:APPDATA\Sublime Text\Packages\User"
# $dotfilesScriptPath = "$env:HOME\.dotfiles\.config\sublime\User"
# New-Item -Path $profileScriptPath -ItemType SymbolicLink -Value $dotfilesScriptPath -Force
# nvm/nodejs

# lazygit
# LinkDotfiles `
#     -program "" `
#     -dotfilesPath ".config\lazygit" `
#     -targetPath "$env:APPDATA\lazygit"

# scoop install nvm
# nvm install node
# nvm use newest
#
# # C++ build tool
# winget search buildtools
# winget install --id=Microsoft.VisualStudio.2022.BuildTools  -e
#
# # uv
# powershell -ExecutionPolicy ByPass -c "irm https://astral.sh/uv/install.ps1 | iex"
# $env:Path = "$HOME\.local\bin;$env:Path"
# uv --version
# # add python
# $env:PATH = "$HOME\AppData\Roaming\uv\python;" + $env:PATH
#
# # bun
# powershell -c "irm bun.sh/install.ps1 | iex"
#
# # dotnet
# winget search Microsoft.DotNet
# winget install Microsoft.DotNet.SDK.9
#
# # check rustup
# $rust_link = "https://www.rust-lang.org/tools/install"
# Writ-Host "rust: $rust_link"
#
# # docker
# winget install -e --id Docker.DockerDesktop
#
# # fzf tool (test)
# $startupFolderPath = "$env:LOCALAPPDATA\television"
# $dotfilesAHKPath = "$HOME\.dotfiles\.config\television"
# New-Item -Path $startupFolderPath -ItemType SymbolicLink -Value $dotfilesAHKPath -Force
#
# UV Python configuration
# Install-ProgramAndLinkDotfiles `
#     -program "" `
#     -dotfilesPath ".config\uv\uv.toml" `
#     -targetPath "$HOME\.config\uv\uv.toml"
#
# # Bun configuration
# Install-ProgramAndLinkDotfiles `
#     -program "" `
#     -dotfilesPath ".config\bun\bunfig.toml" `
#     -targetPath "$HOME\.bunfig.toml"
#
# # Cargo (Rust) configuration
# Install-ProgramAndLinkDotfiles `
#     -program "" `
#     -dotfilesPath ".config\cargo\config.toml" `
#     -targetPath "$HOME\.cargo\config.toml"
#
# # NPM configuration
# Install-ProgramAndLinkDotfiles `
#     -program "" `
#     -dotfilesPath ".config\npm\.npmrc" `
#     -targetPath "$HOME\.npmrc"
#
# # Git configuration (if not already done)
# Install-ProgramAndLinkDotfiles `
#     -program "" `
#     -dotfilesPath ".config\git\.gitconfig" `
#     -targetPath "$HOME\.gitconfig"
#
# # Pip configuration
# Install-ProgramAndLinkDotfiles `
#     -program "" `
#     -dotfilesPath ".config\pip\pip.conf" `
#     -targetPath "$env:APPDATA\pip\pip.ini
