# Neovim Configuration

>[!NOTE]
> it will be update date by date, since I will do some change that during my daily use.

## Info

I start with [kickstart] and [NvChad UI] to configure my personal [Neovim] setting. If you would like to use my configuration, you can clone it and follow the command below. Feel free to use it and make ths issue or PRs.

## MyNeovim

## Setup
**Requirements**
  Neovim >= 0.11
- git >= 2.47.1
- curl
- fzf
- ripgrep
- fd || fdfind
- lazygit `option`

**Getting Started**
```bash
# remove the old Neovim configuration
rm -rf ~/.config/nvim
rm -rf ~/.local/state/nvim
rm -rf ~/.local/share/nvim
# install and clone
git clone  https://github.com/WeiTing1991/myDotfiles/tree/main/.config/nvim ~/.config/nvim
```

```powershell
rm -Force ~\AppData\Local\nvim
rm -Force ~\AppData\Local\nvim-data
```

> In Neovim run `:MasonToolInstall` to install the extra(linter and formater).

## Config

> [!NOTE]
> Still under documenting

```sh
```

**Core**

**UI**

**Package manager**
- [lazy.nvim](https://github.com/folke/lazy.nvim) - no question! From folke.

**Navigation**
<!-- - [telescope](https://github.com/nvim-telescope/telescope.nvim) - fuzzy finder, it needs [ripgrep] and [fd]. -->
- [oil](https://github.com/stevearc/oil.nvim) - really useful for creating and modifying files as normal buffer. It is really similar to Dired in Emacs.

**Treesitter&Lsp**
- [treesitter](https://github.com/nvim-treesitter/nvim-treesitter)

**Editor tool**
  <!-- - [todo-comments](): -->
  - [indent-blankline]():
  - [mini.indentscope]():
  - [virt-column]():
  - [comment]():
  - [mini.pairs]():
  - [mini.ai]():

**Misc**
  - [gitsigns](https://github.com/lewis6991/gitsigns.nvim) -
  - [copilot.lua](https://github.com/zbirenbaum/copilot.lua)
  - [trouble](https://github.com/folke/trouble.nvim)

**Note taking**
<!--   - [render markdown](https://github.com/MeanderingProgrammer/render-markdown.nvim) -->
<!--   - [markdown-preview](https://github.com/iamcco/markdown-preview.nvim) -->
<!---->

**Support Language**

  - [ ]  C/C++ `- extra plugins`
	- [x]  C#
  - [x]  Python
  - [x]  JavaScript/Typescript
  - [ ]  Go
  - [ ]  Java


### Keybindings

More information you can find [here](https://weiting1991.github.io/weitingworks/posts/240927_neovim_setup/).

### LSP

More information you can find [here](https://weiting1991.github.io/weitingworks/posts/240927_neovim_setup/).

### Features

<!-- link -->
[kickstart]: https://github.com/nvim-lua/kickstart.nvim
[Neovim]: https://neovim.io/
[GNU stow]: https://www.gnu.org/software/stow/manual/stow.html
[ripgrep]:
[fd]:


https://github.com/stevearc/overseer.nvim
