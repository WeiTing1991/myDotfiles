$startupFolderPath = "$HOME\.gitconfig"
$dotfilesAHKPath = "$HOME\.dotfiles\.gitconfig"
New-Item -Path $startupFolderPath -ItemType SymbolicLink -Value $dotfilesAHKPath -Force
