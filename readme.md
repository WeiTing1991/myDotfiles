# Welcome to My Dotfiles

This repository is my dotfiles (always updating) via [GNU stow].

## Installation

Please find the installation guide for [GNU stow].

```bash
git clone --recurse-submodules https://github.com/WeiTing1991/myDotfiles.git .dotfiles
cd .dotfiles && stow .
```

> If you already cloned without `--recurse-submodules`:
>
> ```bash
> git submodule update --init --recursive
> ```

## Tools

### Neovim

Neovim config lives in a separate repo as a submodule:
[wtnvim](https://github.com/WeiTing1991/wtnvim) → `.config/nvim/`

I start with [kickstart] to configure my personal [Neovim] setting. If you would like to use my configuration, you can clone it and follow the command below. Feel free to use it and let me know if these is any issue.

**More information** find [here](https://github.com/WeiTing1991/wtnvim)

### Terminal and shell

<!--- Ghostty
  I am using [Ghostty](https://github.com/ghostty-org/ghostty) as my terminal align with Tmux, which is a faster terminal specially when it renders images. And no lag time and fast refresh, the main reason I use switch to it.

```bash
cd ~/.dotfiles/
stow -t ghostty ~/.config
```-->

- WezTerm

	I use [WezTerm](https://wezfurlong.org/wezterm/) as my terminal with person configurations, which makes it similar TUMX. It is an amazing GPU terminal emulator for cross-platform system.

```bash
cd ~/.dotfiles/
#stow -t wezterm ~/.config
```

### JetBrains IDE

Check [ideavimrc](./.ideavimrc) for my personal configuration.

### VSCode/Zed

SoSometimes I use VSCode/Zed for just for quick edit on Window. I have make it similar to my `Neovim` workflow.

`.vscode/` is a single config shared by both platforms. Every keybinding uses
`ctrl+*` rather than `cmd+*`, and Windows-only settings are namespaced
(`terminal.integrated.defaultProfile.windows`), so macOS ignores them. Anything
that genuinely has to differ belongs in a `when` clause using the `isWindows` /
`isMac` context keys, not in a second copy of the file.

#### Sync without account

```powershell
# Only for Windows - or just run windows\install.ps1, which does this
$codeUser = "$env:APPDATA\Code\User"
foreach ($file in @("settings.json", "keybindings.json", "tasks.json")) {
    New-Item -Path "$codeUser\$file" -ItemType SymbolicLink `
        -Value "$HOME\.dotfiles\.vscode\$file" -Force
}
```

```bash
# Only for mac
cd ~/.dotfiles
rm -f ~/Library/Application\ Support/Code/User/{settings,keybindings,tasks}.json
stow -t "$HOME/Library/Application Support/Code/User" .vscode
```

#### Plugins List

### OS keybinding tool
- Windows
  - [AUTOHOTKEY](https://www.autohotkey.com/): win+R > shell:startup put the`*.ahk into the startup folder`
  
- MacOS
  - [Hammerspoon](https://www.hammerspoon.org/faq/)
  - [Nerd Font](https://www.nerdfonts.com/font-downloads)

### Emacs (**Deprecated**) just for storing my old configuration.

Please find more information [here](https://github.com/WeiTing1991/wtemacs).
