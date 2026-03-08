# Neovim Configuration

> [!NOTE]
> It will be updated from time to time, since I make changes during my daily use.

## Info

Standalone Neovim 0.11 configuration with native LSP auto-discovery and lazy.nvim plugin management. Performance-optimized with aggressive lazy loading (target < 50ms startup).

## MyNeovim

### Architecture & Structure

Modular Design:

- `init.lua` -> `core/` -> `plugins/` flow
- Native Neovim 0.11 LSP config via `lsp/<server>.lua` auto-discovery
- Each plugin category has its own spec file

Core Organization:

- `core/` - Basic Neovim settings, keymaps, autocmds, utils
- `lsp/` - Per-server LSP configurations (auto-discovered by Neovim 0.11)
- `plugins/` - Plugin specifications with inline configs
- `overseer/` - Task runner templates
- `snippets/` - Language-specific code snippets

Plugin Management:

- Uses lazy.nvim for lazy loading
- Only 2 plugins load at startup (gruvbox + snacks dashboard)
- Cross-platform compatibility (macOS and Windows)

You can find more detail [here](./structure.md)

### Plugin Ecosystem

**Essential Tools:**

- File Management: Oil, NeoTree
- Fuzzy Finding: FZF-Lua
- LSP: Native Neovim 0.11 + Mason, blink.cmp completion
- Formatting: conform.nvim (async)
- Git Integration: Gitsigns, Fugitive, LazyGit
- AI Tools: Copilot, OpenCode
- Spell check with fastspell

You can find all the plugins [here](./plugins.md)

### Keybindings

All keymaps are defined either in `core/keymaps.lua` (core motions) or inline within plugin specs (plugin-specific keymaps).

Detailed keybinding documentation: [Neovim Keymaps Guide](./keybindings.md)

## Setup

**Requirements**

- Neovim >= 0.11
- git
- fzf
- ripgrep
- fd || fdfind
- lazygit `optional`

**Getting Started**

```bash
# remove the old Neovim configuration
rm -rf ~/.config/nvim
rm -rf ~/.local/state/nvim
rm -rf ~/.local/share/nvim

# install and clone
git clone https://github.com/WeiTing1991/myDotfiles.git ~/.config/nvim-temp
cp -r ~/.config/nvim-temp/.config/nvim ~/.config/nvim
rm -rf ~/.config/nvim-temp
```

```powershell
# Windows PowerShell - remove old Neovim configuration
Remove-Item -Recurse -Force "$env:LOCALAPPDATA\nvim" -ErrorAction SilentlyContinue
Remove-Item -Recurse -Force "$env:LOCALAPPDATA\nvim-data" -ErrorAction SilentlyContinue

# install and clone
git clone https://github.com/WeiTing1991/myDotfiles.git "$env:LOCALAPPDATA\nvim-temp"
Copy-Item -Recurse "$env:LOCALAPPDATA\nvim-temp\.config\nvim" "$env:LOCALAPPDATA\nvim"
Remove-Item -Recurse -Force "$env:LOCALAPPDATA\nvim-temp"
```

> In Neovim run `:MasonToolsInstallSync` to install LSP, linter, and formatter

### LSP Servers

| Language | Server |
|----------|--------|
| Lua | lua_ls |
| Bash/Sh/Zsh | bashls |
| Markdown | marksman |
| JSON/JSONC | jsonls (schemastore) |
| YAML | yamlls (schemastore) |
| TOML | taplo |
| Dockerfile | dockerls |
| Docker Compose | docker_compose_language_service |
| Python | basedpyright, ruff |
| C/C++ | clangd |
| CMake | neocmakelsp |
| C# | roslyn |
| XML | lemminx |

### Formatters & Linters

| Tool | Language |
|------|----------|
| stylua | Lua |
| ruff_format | Python |
| prettier | JSON, JSONC, YAML, Markdown |
| actionlint | GitHub Actions |
| shfmt | Shell (sh, bash, zsh) |
| clang-format | C/C++ |
| csharpier | C# |

### Language Support Status

- [x] C/C++
- [x] C#
- [x] Python
- [x] Lua
- [x] Bash/Shell
- [x] JSON/YAML/TOML
- [x] Docker
- [x] Markdown
- [ ] JavaScript/TypeScript (partial)
- [ ] Go
