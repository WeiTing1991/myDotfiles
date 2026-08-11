```sh
~/.config/nvim/
├── lua/                          # Main Lua configuration
│   ├── core/                     # Core Neovim settings
│   │   ├── autocmds.lua          # Auto commands
│   │   ├── keymaps.lua           # General keybindings
│   │   ├── options.lua           # Neovim options/settings
│   │   └── utils.lua             # Platform detection, window toggle
│   ├── overseer/                 # Task runner templates
│   │   ├── component/
│   │   │   └── on_complete_trouble.lua
│   │   └── template/
│   │       ├── cpp/build.lua
│   │       ├── csharp/build.lua
│   │       ├── csharp/format.lua
│   │       └── lua/format.lua
│   └── plugins/                  # Plugin specifications
│       ├── ai.lua                # AI tools (Copilot, OpenCode)
│       ├── completion.lua        # blink.cmp + LuaSnip
│       ├── editor.lua            # Editor plugins (surround, autopairs, comment, mini.clue, fastspell)
│       ├── formatting.lua        # conform.nvim formatters
│       ├── git.lua               # Git integration (fugitive, gitsigns, lazygit, diffview)
│       ├── lsp.lua               # LSP setup (mason, lspconfig, fidget, lazydev)
│       ├── navigation.lua        # File finding (fzf-lua, oil, neo-tree)
│       ├── tools.lua             # Dev tools (trouble, outline, refactoring, overseer)
│       ├── treesitter.lua        # Treesitter + textobjects + context
│       └── ui.lua                # UI (gruvbox, snacks, lualine, dropbar, virt-column)
├── lsp/                          # Neovim 0.11 native LSP configs (auto-discovered)
│   ├── basedpyright.lua
│   ├── bashls.lua
│   ├── clangd.lua
│   ├── docker_compose_language_service.lua
│   ├── dockerls.lua
│   ├── jsonls.lua
│   ├── lemminx.lua
│   ├── lua_ls.lua
│   ├── marksman.lua
│   ├── neocmakelsp.lua
│   ├── ruff.lua
│   ├── taplo.lua
│   └── yamlls.lua
├── snippets/                     # Code snippets (VSCode format)
├── spell/                        # Spell check dictionaries
├── .stylua.toml                  # Lua formatter config
├── filetype.lua                  # File type detection
└── init.lua                      # Main entry point (vim.loader, lazy bootstrap)
```
