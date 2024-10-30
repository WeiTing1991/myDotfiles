# oh-my-posh init pwsh --config "$env:posh_themes_path\pure.omp.json" | Invoke-Expression
$env:VIRTUAL_ENV_DISABLE_PROMPT = 1

Set-Alias touch New-Item
# Set-Alias zed C:\Users\weitingchen\zed\target\release\zed.exe

Set-Alias n nvim
Set-Alias em emacs
Set-Alias e exit
Set-Alias x clear

function g ($command) {cd C:\Users\weitingchen\work\99_Github}
function ob ($command) {cd C:\Users\weitingchen\iCloudDrive\iCloud~md~obsidian\weitingchen }

function gdrive ($command) {cd G:\.shortcut-targets-by-id\1AhcyENBzXs13kiaeR7txKn5xdpV0sGGn\002_Projects\003_InnoSuisse_MüllerSteinag }
function usi ($command) {cd C:\Users\weitingchen\work\01_USI}

function which ($command){
  Get-Command -Name $command -ErrorAction SilentlyContinue |
  Select-Object -ExpandProperty Path -ErrorAction SilentlyContinue
}