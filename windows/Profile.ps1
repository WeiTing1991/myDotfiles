# Encoding
try {
    [Console]::InputEncoding  = [System.Text.Encoding]::UTF8
    [Console]::OutputEncoding = [System.Text.Encoding]::UTF8
    chcp 65001 > $null
} catch {}

# ============================================
# Module Loading (with error checking)
# ============================================

$modules = @('PSReadLine', 'posh-git')
foreach ($module in $modules) {
    if (Get-Module -ListAvailable -Name $module) {
        Import-Module $module -ErrorAction SilentlyContinue
    }
}

# ============================================
# Starship Prompt (call ONCE only!)
# ============================================
if (Get-Command starship -ErrorAction SilentlyContinue) {
    $ENV:STARSHIP_PRECMD_ASYNC = "1"
    Invoke-Expression (&starship init powershell)
}

# WezTerm cwd tracking (OSC 7) - called by starship before each prompt
function Invoke-Starship-PreCommand {
    if ($PWD.Provider.Name -ne 'FileSystem') { return }
    $cwd = $PWD.Path -replace '\\', '/'
    Write-Host -NoNewline "`e]7;file://$env:COMPUTERNAME/$cwd`a"
}

# ============================================
# PSReadLine Configuration
# ============================================
Set-PSReadLineOption -PredictionSource History
Set-PSReadLineOption -PredictionViewStyle InlineView
Set-PSReadLineOption -EditMode Emacs
Set-PSReadLineOption -BellStyle None

# Keybindings
$keybindings = @{
    "Ctrl+a"       = "BeginningOfLine"
    "Ctrl+e"       = "EndOfLine"
    "Ctrl+k"       = "ForwardDeleteLine"
    "Ctrl+u"       = "BackwardDeleteLine"
    "Ctrl+r"       = "ReverseSearchHistory"
    "Ctrl+l"       = "ClearScreen"
    "Ctrl+w"       = "BackwardDeleteWord"
    "Alt+f"        = "ForwardWord"
    "Alt+b"        = "BackwardWord"
    "Alt+d"        = "DeleteWord"
    "Alt+Backspace"= "BackwardDeleteWord"
    "Alt+p"        = "HistorySearchBackward"
    "Alt+n"        = "HistorySearchForward"
    "Tab"          = "MenuComplete"
}

$keybindings.GetEnumerator() | ForEach-Object {
    Set-PSReadLineKeyHandler -Key $_.Key -Function $_.Value
}

# ============================================
# Environment & Aliases
# ============================================
$ENV:EDITOR = 'nvim'

Set-Alias c clear
Set-Alias python3 python
Set-Alias pip3 pip

# ============================================
# Functions
# ============================================

function n { nvim @args }
function e { exit }
function pj { cd $HOME\project\ }

# Lazy load uv Python
function uv-init {
    $uvPython = uv python find 2>$null
    if ($uvPython) {
        $uvPythonDir = Split-Path $uvPython
        $env:PATH = "$uvPythonDir;$env:PATH"
        Write-Host "✓ uv python loaded" -ForegroundColor Green
    }
}

# Git browser function
function GitBrowser {
    try {
        $url = git remote -v | Select-Object -First 1 | ForEach-Object { ($_ -split '\s+')[1] }
        if (-not $url) { Write-Error "No git remote found"; return }

        $url = $url -replace 'git@github\.com:', 'https://github.com/'
        $url = $url -replace '\.git$', ''

        $branch = git rev-parse --abbrev-ref HEAD 2>$null
        if ($branch -and $branch -ne 'HEAD') {
            $url = "$url/tree/$branch"
        }

        Start-Process $url
    } catch {
        Write-Error "Failed to open git repo: $_"
    }
}
Set-Alias git-browse GitBrowser

# Lazy load conda
function conda-init {
    $condaPath = "$env:USERPROFILE\miniforge3\Scripts\conda.exe"
    if (Test-Path $condaPath) {
        (& $condaPath shell.powershell hook) | Out-String | Invoke-Expression
        Write-Host "✓ conda loaded" -ForegroundColor Green
    } else {
        Write-Warning "Conda not found at $condaPath"
    }
}

# ============================================
# Optional: Load on demand
# ============================================
# Uncomment below if you want lazy loading:
# function conda { conda-init; & conda @args }
# function uv { uv-init; & uv @args }
