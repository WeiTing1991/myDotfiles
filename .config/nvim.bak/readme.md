# Neovim Configuration

> [!NOTE]
> It will be updated from time to time, since I make changes during my daily use.

## Info

I start with [kickstart] and [NvChad UI] to configure my personal [Neovim] setting. If you would like to use my configuration, you can clone it and follow the command below. Feel free to use it and make the issue or PRs.

## MyNeovim

![WTNeovim](./doc/wtnvim_home.png)
![WTNeovim_Editore](./doc/wtnvim_preview.png)

### Architecture & Structure

Modular Design:

- `init.lua` -> `core/` -> `plugins/` flow
- Each plugin has its own configuration file

Core Organization:

- `core/` - Basic Neovim settings and keymaps
- `lsp/` - Language server configurations
- `plugins/` - Plugin specifications and configs
- `overseer/` - Task runner templates
- `snippets/` - Language-specific code snippets

Plugin Management:

- Uses lazy.nvim for lazy loading
- Individual plugin configs in `plugins/configs/`
- Cross-platform compatibility (Windows and Mac)

You can find more detail [here](./structure.md)

### Plugin Ecosystem

**Essential Tools:**

- File Management: Oil, NeoTree
- Fuzzy Finding: FZF-Lua
- LSP: Full setup with Mason, blink.cmp completion
- Git Integration: Gitsigns, Fugitive, LazyGit
- AI Tools: Copilot, OpenCode, Claude Code (sidekick)
- Spell check with fastspell

You can find all the plugins [here](./plugins.md)

### Keybindings

The main core keymaps is in `core/keymaps`, it has basic neovim motion and some useful keymaps.
The LSP keymaps is in `lsp/keymaps`, it is all about lsp stuffs.
The custom keymaps is under root folder `user_keymaps`, it contains user override and the plugins keymaps.

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
| ruff | Python |
| prettier | JSON, YAML, Markdown, HTML |
| actionlint | GitHub Actions |
| shfmt | Shell |
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

[kickstart]: https://github.com/nvim-lua/kickstart.nvim
[Neovim]: https://neovim.io/
[NvChad UI]: https://nvchad.com/
