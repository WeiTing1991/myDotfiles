# Neovim Configuration

>[!NOTE]
> it will be update date by date, since I will do some change that during my daily use.

## Info

I start with [kickstart] and [NvChad UI] to configure my personal [Neovim] setting. If you would like to use my configuration, you can clone it and follow the command below. Feel free to use it and make ths issue or PRs.

## MyNeovim
![](/Users/weitingchen/Desktop/Snipaste_2025-03-02_04-28-25.jpg)


## Setup

**Requirements**
  Neovim >= 0.10.2
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
> In Neovim run `:MasonToolInstall` to install the extra(linter and formater).

## Config

> [!NOTE]
> Still under documenting

```sh
├── init.lua
├── lua
│   ├── chadrc.lua
│   ├── configs
│   │   ├── achive
│   │   │   └── telescope.lua
│   │   ├── copilot.lua
│   │   ├── fzf.lua
│   │   ├── indentscope.lua
│   │   ├── lsp
│   │   │   ├── configs
│   │   │   │   ├── extra.lua
│   │   │   │   └── server.lua
│   │   │   ├── lsp-cmp.lua
│   │   │   ├── lsp-format.lua
│   │   │   ├── lsp-init.lua
│   │   │   └── lsp-ui.lua
│   │   ├── nvimtree.lua
│   │   ├── oil.lua
│   │   ├── staline.lua
│   │   ├── todo.lua
│   │   └── treesitter.lua
│   ├── core
│   │   ├── autocmds.lua
│   │   ├── init.lua
│   │   ├── keymappings.lua
│   │   ├── lazy.lua
│   │   └── options.lua
│   ├── custom_plugins
│   │   └── toggle_maximize_window.lua
│   ├── lsp_keymaps.lua
│   ├── mapping.lua
│   └── plugin
│       ├── editor.lua
│       ├── init.lua
│       ├── lsp.lua
│       ├── misc.lua
│       ├── note_taking.lua
│       └── tools.lua
├── snippets
└── spell
```

**Core**
    `autocmds.lua`
    `config.lua`
    `init.lua`
    `keymaps.lua`
    `lazy.lua`

**UI**
- [Nvchard](https://github.com/NvChad/ui) - better ui, theme, transparent integration.

**Package manager**
- [lazy.nvim](https://github.com/folke/lazy.nvim) - no question! From folke.

**Navigation**
- [telescope](https://github.com/nvim-telescope/telescope.nvim) - fuzzy finder, it needs [ripgrep] and [fd].
- [oil](https://github.com/stevearc/oil.nvim) - really useful for creating and modifying files as normal buffer. It is really similar to Dired in Emacs.

**Treesitter&Lsp**
- [treesitter](https://github.com/nvim-treesitter/nvim-treesitter)

**Editor tool**
  - [todo-comments]():
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

  - [x]  C/C++ `- extra plugins`
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

