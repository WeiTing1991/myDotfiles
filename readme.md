# Welcome to My Dotfiles

This repository is my dotfiles (always updating) via [GNU stow].

## Installation

Please find the installation guide for [GNU stow].

```bash
git clone --recurse-submodules https://github.com/WeiTing1991/myDotfiles.git .dotfiles
cd .dotfiles && stow .
```

> If you already cloned without `--recurse-submodules`:
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

- Ghostty
  I am using [Ghostty](https://github.com/ghostty-org/ghostty) as my terminal align with Tmux, which is a faster terminal specially when it renders images. And no lag time and fast refresh, the main reason I use switch to it.

```bash
cd ~/.dotfiles/
stow -t ghostty ~/.config
```

- WezTerm

  On Windows I use [WezTerm](https://wezfurlong.org/wezterm/) as my terminal with person configurations, which makes it similar TUMX. It is an amazing GPU terminal emulator for cross-platform system.

```bash
cd ~/.dotfiles/
#stow -t wezterm ~/.config
```

### VSCode/Zed

Sometimes I use VSCode/Zed for just for quick edit on Window. I have make it similar to my Neovim workflow.


#### Sync

```bash
# only for window
New-Item -Path $env:USERPROFILE\AppData\Roaming\Code\User\settings.json -ItemType SymbolicLink -Value $env:USERPROFILE\.dotfiles\.vscode\settings.json -Force
New-Item -Path $env:USERPROFILE\AppData\Roaming\Code\User\keybindings.json -ItemType SymbolicLink -Value $env:USERPROFILE\.dotfiles\.vscode\keybindings.json -Force
```

```bash
# Only for mac
rm ~/Library/Application\ Support/Code/User/keybindings.json
stow -t "$(echo ~/Library/Application\ Support/Code/User)" .vscode
```

#### Plugins List

### keybinding tool
- Windows
	- [AUTOHOTKEY](https://www.autohotkey.com/): win+R > shell:startup put the`*.ahk into the startup folder`
- MacOS
	- [Hammerspoon](https://www.hammerspoon.org/faq/)
	- [Nerd Font](https://www.nerdfonts.com/font-downloads)

### Emacs (Deprecated) just for storing my old configuration.
Please find more information [here](https://github.com/WeiTing1991/wtemacs).

<!-- link -->
