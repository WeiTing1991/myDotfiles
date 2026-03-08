# Import-Module PSReadLine
Import-Module posh-git
$GitPromptSettings.EnablePromptStatus = $false

# Load Starship (handles the prompt)
Invoke-Expression (&starship init powershell)


Invoke-Expression (&starship init powershell)

# Add WezTerm cwd tracking AFTER starship init
$ENV:STARSHIP_PRECMD_ASYNC = "1"

# WezTerm integration
function Invoke-Starship-PreCommand {
    $cwd = $PWD.Path -replace "\\", "/"
    Write-Host -NoNewline "`e]7;file://$env:COMPUTERNAME/$cwd`a"
}

# Windows Terminal integration
# function Invoke-Starship-PreCommand {
#     $loc = $executionContext.SessionState.Path.CurrentLocation
#     if ($loc.Provider.Name -eq "FileSystem") {
#         $host.ui.Write("`e]9;9;`"$($loc.ProviderPath)`"`e\")
#     }
# }


# Predictions
Set-PSReadLineOption -PredictionSource History
Set-PSReadLineOption -PredictionViewStyle InlineView
Set-PSReadLineOption -EditMode Emacs
Set-PSReadLineOption -BellStyle None

# Keybindings
Set-PSReadLineKeyHandler -Key "Ctrl+a"  -Function BeginningOfLine      # auto
Set-PSReadLineKeyHandler -Key "Ctrl+e"  -Function EndOfLine            # auto
Set-PSReadLineKeyHandler -Key "Ctrl+k"  -Function ForwardDeleteLine    # auto
Set-PSReadLineKeyHandler -Key "Ctrl+u"  -Function BackwardDeleteLine   # auto
Set-PSReadLineKeyHandler -Key "Ctrl+r"  -Function ReverseSearchHistory # auto
Set-PSReadLineKeyHandler -Key "Ctrl+l"  -Function ClearScreen          # auto
Set-PSReadLineKeyHandler -Key "Ctrl+w"  -Function BackwardDeleteWord   # auto

Set-PSReadLineKeyHandler -Key "Alt+f"         -Function ForwardWord
Set-PSReadLineKeyHandler -Key "Alt+b"         -Function BackwardWord
Set-PSReadLineKeyHandler -Key "Alt+d"         -Function DeleteWord
Set-PSReadLineKeyHandler -Key "Alt+Backspace" -Function BackwardDeleteWord
Set-PSReadLineKeyHandler -Key "Alt+p"         -Function HistorySearchBackward
Set-PSReadLineKeyHandler -Key "ALt+n"         -Function HistorySearchForward
Set-PSReadLineKeyHandler -Key Tab -Function MenuComplete
# Set-PSReadLineKeyHandler -Key Ctrl+f -Function AcceptSuggestion

$ENV:EDITOR = 'nvim'

Set-Alias c clear
function n ($command) { nvim }
function e ($command) { exit }

# Python
function uv-init {
    $uvPython = uv python find
    if ($uvPython) {
        $uvPythonDir = Split-Path $uvPython
        $env:PATH = "$uvPythonDir;" + $env:PATH
    }
    $uvToolsDir = Join-Path $env:APPDATA "uv\tools"
    if (Test-Path $uvToolsDir) {
        $env:PATH = "$uvToolsDir;" + $env:PATH
    }
    Write-Host "uv python loaded."
}

Set-Alias python3 python
Set-Alias pip3 pip

# function gdrive ($command) {cd G:\.shortcut-targets-by-id\1AhcyENBzXs13kiaeR7txKn5xdpV0sGGn\002_Projects\003_InnoSuisse_MüllerSteinag }
# function usi ($command) {cd \work\01_USI}
function pj ($command) { cd $HOME\project\ }

# function where ($command) {
#   Get-Command -Name $command -ErrorAction SilentlyContinue
#   # Select-Object -ExpandProperty Path -ErrorAction SilentlyContinue
# }

function GitBrowser {
  $url = git remote -v | Select-Object -First 1 | ForEach-Object { ($_ -split '\s+')[1] }
  $url = $url -replace 'git@github\.com:', 'https://github.com/'
  $url = $url -replace '\.git$', ''
  Start-Process $url
}
Set-Alias -Name git-browse -Value GitBrowser

# Starship prompt
Invoke-Expression (&starship init powershell)

# scoop search
# . ([ScriptBlock]::Create((& scoop-search --hook | Out-String)))
function scoop-s { scoop-search @args }

#region conda initialize
# !! Contents within this block are managed by 'conda init' !!
function conda-init {
    (& "C:\Users\WeiTing\miniforge3\Scripts\conda.exe" "shell.powershell" "hook") | Out-String | ? { $_ } | Invoke-Expression
    Write-Host "conda loaded."
}
# If (Test-Path "C:\Users\WeiTing\miniforge3\Scripts\conda.exe") {
#   (& "C:\Users\WeiTing\miniforge3\Scripts\conda.exe" "shell.powershell" "hook") | Out-String | ? { $_ } | Invoke-Expression
# }
#endregion
# MSBuild
# $env:MSBUILD = "C:\Program Files\Microsoft Visual Studio\2022\Professional\MSBuild\Current\Bin\MSBuild.exe"  # <-- UPDATE THIS with your actual path
# function msbuild { & $env:MSBUILD @args }
# $env:MSBUILD = & "${env:ProgramFiles(x86)}\Microsoft Visual Studio\Installer\vswhere.exe" -latest -requires Microsoft.Component.MSBuild -find MSBuild\**\Bin\MSBuild.exe 2>$null | Select-Object -First 1
# function msbuild { & $env:MSBUILD @args }
