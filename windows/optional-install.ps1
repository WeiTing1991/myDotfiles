# Warp
Install-ProgramAndLinkDotfiles `
-program "Warp.Warp" `
-dotfilesPath ".warp\keybindings.yaml" `
-targetPath "$env:LOCALAPPDATA\warp\Warp\config\keybindings.yaml"

# Starship
Install-ProgramAndLinkDotfiles `
    -program "Starship.Starship" `
    -dotfilesPath ".config\starship.toml" `
    -targetPath "$HOME\.config\starship.toml"