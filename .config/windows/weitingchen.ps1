# Import the PSReadLine module
Import-Module PSReadLine

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
Set-PSReadLineKeyHandler -Key Ctrl+w -Function DeleteCharOrExit

# Initialize Oh-My-Posh with a theme
oh-my-posh init pwsh --config "$env:POSH_THEMES_PATH\paradox.omp.json" | Invoke-Expression

$env:VIRTUAL_ENV_DISABLE_PROMPT = 1
Set-Location $HOME

Set-Alias c clear

function n ($command) {goneovim .}
function e ($command) {exit}

#function gdrive ($command) {cd G:\.shortcut-targets-by-id\1AhcyENBzXs13kiaeR7txKn5xdpV0sGGn\002_Projects\003_InnoSuisse_MüllerSteinag }
function usi ($command) {cd C:\Users\weitingchen\work\01_USI}

function which ($command){
  Get-Command -Name $command -ErrorAction SilentlyContinue |
  Select-Object -ExpandProperty Path -ErrorAction SilentlyContinue
}
