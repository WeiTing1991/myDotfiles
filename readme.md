# Welcome to my dotfiles

This repository is my dotfiles (for now) for my windows and mac via [GNU stow].

## stow 

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

<details><summary>The plugins I am using now <p></summary>

Latest version of the Neovim setup and plugins.
Package manager

- [lazy.nvim](https://github.com/folke/lazy.nvim)

Parsing

- [treesitter](https://github.com/nvim-treesitter/nvim-treesitter)

LSP and Auto completion

- [lspconfig](https://github.com/neovim/nvim-lspconfig)
- [mason](https://github.com/williamboman/mason.nvim)
- [mason-lspconfig](https://github.com/williamboman/mason-lspconfig.nvim)
- [lspsaga](https://github.com/glepnir/lspsaga.nvim)
- [nvim-cmp](https://github.com/hrsh7th/nvim-cmp)
- [none-ls](https://github.com/nvimtools/none-ls.nvim)

Navigation

- [telescope](https://github.com/nvim-telescope/telescope.nvim)
- [nvim-tree](https://github.com/nvim-tree/nvim-tree.lua)
- [oil]()

Editor tool/ Git/ notes taking

- [copilot](https://github.com/zbirenbaum/copilot.lua)
- [gitsigns](https://github.com/lewis6991/gitsigns.nvim)
- [render markdown](https://github.com/MeanderingProgrammer/render-markdown.nvim)
- [trouble](https://github.com/folke/trouble.nvim)

UI

- [lualine](https://github.com/nvim-lualine/lualine.nvim)
- [mini-indentscope](https://github.com/echasnovski/mini.indentscop)

Debugging

- [nvim-dap](https://github.com/mfussenegger/nvim-dap)
- [nvim-dap-go](https://github.com/leoluz/nvim-dap-go)

Java LSP

- [jdtls](https://github.com/mfussenegger/nvim-jdtls)

</details>

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
