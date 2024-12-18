# Welcome to my dotfiles

## Installation
stow
neovim

## stow

This repository is my dotfiles (for now) for my windows and mac via [GNU stow].


```bash
stow .
```

## Neovim
I start with [kickstart] to configure my personal [Neovim] setting. If you would like to use my configuration, you can clone it and follow the command below. Feel free to use it and let me know if these is any issue.

### Usage
## Terminal
I am using [WezTerm](https://wezfurlong.org/wezterm/) as my terminal with some configurations, which make it similar to tumx. It is an amazing GPU terminal emulator for cross-platform system.

MacOS

```bash
cd YOURGITREPO
mv wezterm ~/.config
```

Windows 11

```bash
cd YOURGITREPO
mv wezterm ~/.config
```

## Vscode

```bash
cd ~/.dotfiles/
stow -t ~/Library/Application\ Support/Code/User ./vscode

```

<!-- Plugins:
vim
editorConfig
Error Lens

custom CSS and JS
nord theme
TODO V2
WhichKey

vsnetrew
FindInFaster
OpenInExternalApp

Prettier -->



<!-- ## Emacs

```bash
# add the submodule
git submodule add https://github.com/WeiTing1991/wtemacs.git .emacs.d

git submodule update --init --recursive

git submodule update --remote --merge

```
Please find more information [here](./Emacs.org).

## Zed
please find more infomation [here](). -->
