# Welcome to my dotfiles

## stow

This repository is my dotfiles (for now) for my windows and mac via [GNU stow].

```bash
stow .
```

## Neovim

I start with [kickstart] to configure my personal [Neovim] setting. If you would like to use my configuration, you can clone it and follow the command below. Feel free to use it and let me know if these is any issue.

### Usage
For MacOS/Linux

```bash
# remove the old config file

mv ~/.config/nvim ~/.config/nvim.bak
rm -rf ~/.local/share/nvim
rm -rf ~/.cache/nvim

cd YOURGITREPO
mv nvim ~/.config

```

For Windows 10/11

```powershell

git clone GitPath $HOME/.mydotfiles

# install the required software and create the symbolic link
./intall.ps1
```

### Configuration

The **plugins** I am using now are listed below. I am using the latest version of the Neovim version 0.10.0.

#### Plugins

<details>
<summary>List<p></summary>

Package manager

  - [lazy.nvim](https://github.com/folke/lazy.nvim)

Parsing

  - [treesitter](https://github.com/nvim-treesitter/nvim-treesitter)

Navigation

  - [telescope](https://github.com/nvim-telescope/telescope.nvim): fuzzy finder
  - [mini-files](https://github.com/nvim-tree/nvim-tree.lua): mini file explorer
  - [oil](https://github.com/stevearc/oil.nvim): really useful for creating and modifying files as
  normal buffer.

Editor tool/ Git/ notes taking

  - [gitsigns](https://github.com/lewis6991/gitsigns.nvim): git signs integration
  - [copilot](https://github.com/zbirenbaum/copilot.lua)
  - [render markdown](https://github.com/MeanderingProgrammer/render-markdown.nvim)
  - [markdown-preview](https://github.com/iamcco/markdown-preview.nvim)
  - [obsidian](https://github.com/iamcco/markdown-preview.nvim)
  - [trouble](https://github.com/folke/trouble.nvim)

UI

  - [lualine](https://github.com/nvim-lualine/lualine.nvim)
  - [mini-indentscope](https://github.com/echasnovski/mini.indentscop)
  - [virt-column](https://github.com/echasnovski/mini.indentscop)
  - [auto-session](https://github.com/echasnovski/mini.indentscop)

</details>

#### LSP

More information you can fine [here](https://weiting1991.github.io/weitingworks/posts/240927_neovim_setup/).

[kickstart]: https://github.com/nvim-lua/kickstart.nvim
[Neovim]: https://neovim.io/
[GNU stow]: https://www.gnu.org/software/stow/manual/stow.html

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

## Emacs

please find more infomation [here]().
```
```

