# Welcome to my dotfiles

## Installation

## stow

This repository is my dotfile (for now) for my windows and mac via [GNU stow].

```bash
stow .

```

## Neovim
I start with [kickstart] to configure my personal [Neovim] setting. If you would like to use my configuration, you can clone it and follow the command below. Feel free to use it and let me know if these is any issue.

**More information** find [here](/.config/nvim/readme.md)

### Usage
## Terminal
I am using [WezTerm](https://wezfurlong.org/wezterm/) as my terminal with some configurations, which make it similar to tumx. It is an amazing GPU terminal emulator for cross-platform system.

**_Mac OS_**

```bash

cd YOURGITREPO
mv wezterm ~/.config

```

_**Windows 10 11**_

```bash

cd YOURGITREPO
mv wezterm ~/.config

```

## Vscode

```bash

cd ~/.dotfiles/
stow -t ~/Library/Application\ Support/Code/User ./vscode

```

![](~/Desktop/01.png)


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
