# Welcome my dotfiles
p
This repository is my dotfiles(still in progress) for my windows and mac via [stow]. \

## Neovim

I start with [kickstart] to configure my personal [Neovim] setting. Feel free to use it and let me
know if it has any issuse. \ If you would like to use my configure, you can clone it and follow the
command below.

For Mac user
```bash
mv ~/.config/nvim ~/.config/nvim.bak
rm -rf ~/.local/share/nvim
rm -rf ~/.cache/nvim

cd YOURGITREPO
mv nvim ~/.config
```

For Windows user (git clone to $HOME)

```powershell
./intall.ps1
```

### Configuration

<details><summary>The plugins I am using now </summary>

- [lazy.nvim](https://github.com/folke/lazy.nvim)

Parsing

- [treesitter](https://github.com/nvim-treesitter/nvim-treesitter)

LSP and Autocompletion

- [lspconfig](https://github.com/neovim/nvim-lspconfig)
- [mason](https://github.com/williamboman/mason.nvim)
- [mason-lspconfig](https://github.com/williamboman/mason-lspconfig.nvim)
- [lspsaga](https://github.com/glepnir/lspsaga.nvim)
- [nvim-cmp](https://github.com/hrsh7th/nvim-cmp)
- [none-ls](https://github.com/nvimtools/none-ls.nvim)

Java LSP

- [jdtls](https://github.com/mfussenegger/nvim-jdtls)

Debugging

- [nvim-dap](https://github.com/mfussenegger/nvim-dap)
- [nvim-dap-go](https://github.com/leoluz/nvim-dap-go)

Navigation

- [telescope](https://github.com/nvim-telescope/telescope.nvim)
- [nvim-tree](https://github.com/nvim-tree/nvim-tree.lua)

Editor tool and Git

- [comment.nvim](https://github.com/numToStr/Comment.nvim)
- [copilot](https://github.com/zbirenbaum/copilot.lua)
- [gitsigns](https://github.com/lewis6991/gitsigns.nvim)

UI

- [lualine](https://github.com/nvim-lualine/lualine.nvim)
- [trouble](https://github.com/folke/trouble.nvim)

</details>

[kickstart]: https://github.com/nvim-lua/kickstart.nvim
[Neovim]: https://neovim.io/

## Terminal

I am using [WezTerm](https://wezfurlong.org/wezterm/) as my terminal with some configuration like tumx.
It is a amazing GPU terminal emulator for cross platform. \

for macOS
```bash
cd YOURGITREPO
mv wezterm ~/.config
```
