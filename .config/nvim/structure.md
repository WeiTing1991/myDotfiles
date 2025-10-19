> [!NOTE]
> Documentation is still being updated

```sh
  ~/.config/nvim/
  ├── lua/                          # Main Lua configuration
  │   ├── core/                     # Core Neovim settings
  │   │   ├── autocmds.lua          # Auto commands
  │   │   ├── init.lua              # Core initialization
  │   │   ├── keymaps.lua           # General keybindings
  │   │   ├── options.lua           # Neovim options/settings
  │   │   └── toogle_max.lua        # Window maximization toggle
  │   ├── lsp/                      # Language Server Protocol
  │   │   ├── formater_linter.lua   # Formatter/linter config
  │   │   ├── keymaps.lua           # LSP-specific keybindings
  │   │   ├── lsp_init.lua          # LSP initialization
  │   │   └── server.lua            # LSP server configurations
  │   ├── overseer/                 # Task runner templates
  │   ├── plugins/                  # Plugin configurations
  │   │   ├── configs/              # Individual plugin configs
  │   │   │   ├── blink.lua         # Blink completion
  │   │   │   ├── fzf.lua           # FZF configuration
  │   │   │   ├── lualine.lua       # Status line
  │   │   │   ├── miniclue.lua      # Keybinding hints
  │   │   │   ├── neotree.lua       # File explorer
  │   │   │   ├── nonels.lua        # None-ls config
  │   │   │   ├── oil.lua           # Oil file manager
  │   │   │   ├── telescope.lua     # Telescope fuzzy finder
  │   │   │   └── treesitter.lua    # Treesitter syntax
  │   │   ├── ai.lua                # AI tools (Copilot, etc.)
  │   │   ├── default.lua           # Default plugin setup
  │   │   ├── editor.lua            # Editor enhancement plugins
  │   │   ├── git.lua               # Git integration plugins
  │   │   ├── lsp.lua               # LSP plugins
  │   │   ├── lsp_enhance.lua       # LSP enhancement plugins
  │   │   ├── snack.lua             # Snacks.nvim dashboard
  │   │   ├── tools.lua             # Development tools
  │   │   └── ui.lua                # UI enhancement plugins
  │   ├── chadrc.lua                # NvChad configuration
  │   ├── icon.lua                  # Icon configurations
  │   └── user_keymaps.lua          # User-defined keymaps
  ├── snippets/                     # Code snippets
  ├── spell/                        # Spell check dictionaries
  ├── .stylua.toml                  # Lua formatter config
  ├── filetype.lua                  # File type detection
  ├── init.lua                      # Main entry point
```
