#Peek error: browser: Failed to spawn 'xdg-open': entity not found Welcome to My Dotfiles

This repository is my dotfiles (always updating) via [GNU stow].

## Installation

Please find the installation guide for [GNU stow].

```bash

git clone https://github.com/WeiTing1991/myDotfiles.git .dotfiles
cd .dotfiles && stow .

```

## Tools

### Neovim

I start with [kickstart] to configure my personal [Neovim] setting. If you would like to use my configuration, you can clone it and follow the command below. Feel free to use it and let me know if these is any issue.

**More information** find [here](/.config/nvim/readme.md)


### Terminal and shell
  - Ghostty

    I am using [Ghostty](https://github.com/ghostty-org/ghostty) as my terminal align with Tmux, which is a faster terminal specially when it renders images. And no lag time and fast refresh, the main reason I use switch to it.

```bash

cd ~/.dotfiles/
stow -t ghostty ~/.config

```

  - WezTerm

    On Windows I use [WezTerm](https://wezfurlong.org/wezterm/) as my terminal with person configurations, which makes it similar to Tumx. It is an amazing GPU terminal emulator for cross-platform system.

```bash

cd ~/.dotfiles/
stow -t wezterm ~/.config

```

### VSCode

Sometimes I use VSCode for just for quick edit on Window (the language i don't use daily such Jupbter). I have make it similar to my Neovim workflow.

- Setup with my config

```bash
# only for window

New-Item -Path $env:USERPROFILE\AppData\Roaming\Code\User\settings.json -ItemType SymbolicLink -Value $env:USERPROFILE\.dotfiles\.vscode\settings.json -Force
New-Item -Path $env:USERPROFILE\AppData\Roaming\Code\User\keybindings.json -ItemType SymbolicLink -Value $env:USERPROFILE\.dotfiles\.vscode\keybindings.json -Force

```

```bash
# only for mac

rm ~/Library/Application\ Support/Code/User/keybindings.json
stow -t "$(echo ~/Library/Application\ Support/Code/User)" .vscode

```
- Plugins
...coming soon

<!-- vim -->
<!-- editorConfig -->
<!-- Error Lens -->
<!---->
<!-- custom CSS and JS -->
<!-- nord theme -->
<!-- TODO V2 -->
<!-- WhichKey -->
<!---->
<!-- vsnetrew -->
<!-- FindInFaster -->
<!-- OpenInExternalApp -->
<!-- Prettier -->

### Emacs (Deprecated) just for storing my old configuration.

Please find more information [here](./Emacs.org).

<!-- ## Zed -->
<!-- please find more information [here](). -->

<!-- link -->
[kickstart]: https://github.com/nvim-lua/kickstart.nvim
[Neovim]: https://neovim.io/
[GNU stow]: https://www.gnu.org/software/stow/manual/stow.html
