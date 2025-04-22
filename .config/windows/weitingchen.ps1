# Import the PSReadLine module
Import-Module PSReadLine

Import-Module posh-git

# Set predictive IntelliSense (PowerShell 7.2+ feature)
Set-PSReadLineOption -PredictionSource History

# Ctrl+Space for IntelliSense suggestions
Set-PSReadLineKeyHandler -Key Ctrl+tab -Function Complete

# Use AcceptSuggestion instead of Complete
Set-PSReadLineKeyHandler -Key Ctrl+f -Function AcceptSuggestion

# Tab completion
#Set-PSReadLineKeyHandler -Key tab -Function MenuComplete

# Searching history with up/down arrows
Set-PSReadLineKeyHandler -Key Ctrl+p -Function HistorySearchBackward
Set-PSReadLineKeyHandler -Key Ctrl+n -Function HistorySearchForward

# Ctrl+d to exit, like in bash
#Set-PSReadLineKeyHandler -Key Ctrl+w -Function DeleteCharOrExit

# Ctrl+B to move backward one character (like in Emacs/Bash)
Set-PSReadLineKeyHandler -Key Ctrl+b -Function BackwardChar

# Ctrl+W to delete the previous word (like in Bash)
Set-PSReadLineKeyHandler -Key Ctrl+w -Function BackwardDeleteWord

# Ctrl+F to move forward one character
Set-PSReadLineKeyHandler -Key Ctrl+f -Function ForwardChar

# Initialize Oh-My-Posh with a theme
oh-my-posh init pwsh --config "$env:POSH_THEMES_PATH\catppuccin_mocha.omp.json" | Invoke-Expression

$env:VIRTUAL_ENV_DISABLE_PROMPT = 1
Set-Location $HOME
$ENV:EDITOR = 'code'

Set-Alias c clear

function n ($command) {nvim}
function e ($command) {exit}

#function gdrive ($command) {cd G:\.shortcut-targets-by-id\1AhcyENBzXs13kiaeR7txKn5xdpV0sGGn\002_Projects\003_InnoSuisse_MüllerSteinag }
function usi ($command) {cd C:\Users\weitingchen\work\01_USI}
function pj ($command) {cd C:\Users\weitingchen\project/}

function which ($command){
  Get-Command -Name $command -ErrorAction SilentlyContinue
  # Select-Object -ExpandProperty Path -ErrorAction SilentlyContinue
}
