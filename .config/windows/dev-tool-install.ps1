# nvm/nodejs
scoop install nvm
nvm install node
nvm use newest

# C++ build tool
winget search buildtools
winget install --id=Microsoft.VisualStudio.2022.BuildTools  -e

# uv
powershell -ExecutionPolicy ByPass -c "irm https://astral.sh/uv/install.ps1 | iex"
$env:Path = "C:\Users\weichen34\.local\bin;$env:Path"
uv --version

# bun
powershell -c "irm bun.sh/install.ps1 | iex"

# dotnet
winget search Microsoft.DotNet
winget install Microsoft.DotNet.SDK.9

# check rustup
$rust_link = "https://www.rust-lang.org/tools/install"
Writ-Host "rust: $rust_link"

# docker
winget install -e --id Docker.DockerDesktop