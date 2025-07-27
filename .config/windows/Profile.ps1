$global:LastOSC7Path = ""
$ExecutionContext.InvokeCommand.PreCommandLookupAction = {
    $current = $PWD.ProviderPath
    if ($current -ne $global:LastOSC7Path) {
        $global:LastOSC7Path = $current
        # Direct ANSI escape without variable assignments
        Write-Host "`e]7;file://$env:COMPUTERNAME/$($current -Replace '\\','/')`e\" -NoNewline
    }
}

Import-Module posh-git
Import-Module PSReadLine

# Use AcceptSuggestion instead of Complete
Set-PSReadLineKeyHandler -Key Ctrl+f -Function AcceptSuggestion

# Searching history with up/down arrows
Set-PSReadLineKeyHandler -Key Ctrl+p -Function HistorySearchBackward
Set-PSReadLineKeyHandler -Key Ctrl+n -Function HistorySearchForward

# Set predictive IntelliSense (PowerShell 7.2+ feature)
Set-PSReadLineOption -PredictionSource History

# Tab completion
Set-PSReadLineKeyHandler -Key tab -Function MenuComplete

# Ctrl+W to delete the previous word (like in Bash)
# Set-PSReadLineKeyHandler -Key Ctrl+w -Function BackwardDeleteWord

# # Ctrl+Space for IntelliSense suggestions
# Set-PSReadLineKeyHandler -Key Ctrl+tab -Function Complete

# # Ctrl+d to exit, like in bash
# #Set-PSReadLineKeyHandler -Key Ctrl+w -Function DeleteCharOrExit

# # Ctrl+B to move backward one character (like in Emacs/Bash)
# Set-PSReadLineKeyHandler -Key Ctrl+b -Function BackwardChar

# # Ctrl+F to move forward one character
# Set-PSReadLineKeyHandler -Key Ctrl+f -Function ForwardChar

# # Initialize Oh-My-Posh with a theme
# oh-my-posh init pwsh --config "$env:POSH_THEMES_PATH\catppuccin_mocha.omp.json" | Invoke-Expression

# $env:VIRTUAL_ENV_DISABLE_PROMPT = 1
# Set-Location $HOME
$ENV:EDITOR = 'nvim'

Set-Alias c clear

function n ($command) {nvim}
function e ($command) {exit}

# add python to PATH
$uvPython = uv python find
if ($uvPython) {
    $uvPythonDir = Split-Path $uvPython
    $env:PATH = "$uvPythonDir;" + $env:PATH
}

# Add UV tools directory
$uvToolsDir = Join-Path $env:APPDATA "uv\tools"
if (Test-Path $uvToolsDir) {
    $env:PATH = "$uvToolsDir;" + $env:PATH
}

Set-Alias python3 python
Set-Alias pip3 pip

#function gdrive ($command) {cd G:\.shortcut-targets-by-id\1AhcyENBzXs13kiaeR7txKn5xdpV0sGGn\002_Projects\003_InnoSuisse_MüllerSteinag }
# function usi ($command) {cd \work\01_USI}
function pj ($command) {cd $HOME\project\}

function which ($command){
  Get-Command -Name $command -ErrorAction SilentlyContinue
  # Select-Object -ExpandProperty Path -ErrorAction SilentlyContinue
}

