function Install-ProgramAndLinkDotfiles {
    param (
        [string]$program,
        [string]$dotfilesPath,
        [string]$targetPath
    )
    # Install the program
    winget install $program

    # Set the full path to the dotfiles
    $sourcePath = "$HOME\.dotfiles\$dotfilesPath"

    # Verify source file exists
    if (-not (Test-Path $sourcePath)) {
        Write-Error "Source file not found: $sourcePath"
        return
    }
    # Create symlink
    New-Item -Path $targetPath -ItemType SymbolicLink -Value $sourcePath -Force
}

function LinkDotfiles {
    param (
        [string]$dotfilesPath,
        [string]$targetPath
    )
    # Set the full path to the dotfiles
    $sourcePath = "$HOME\.dotfiles\$dotfilesPath"

    # Verify source file exists
    if (-not (Test-Path $sourcePath)) {
        Write-Error "Source file not found: $sourcePath"
        return
    }
    # Create symlink
    New-Item -Path $targetPath -ItemType SymbolicLink -Value $sourcePath -Force
}
