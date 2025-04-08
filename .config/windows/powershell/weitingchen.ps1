Import-Module PSReadLine
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
