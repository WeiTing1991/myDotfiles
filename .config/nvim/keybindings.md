# Neovim Keybindings Reference

## Leader Keys
- **Leader**: `<Space>`
- **Local Leader**: `,`

## Core Navigation & Movement

| Mode | Keybinding | Description |
|------|------------|-------------|
| `n` | `j`/`k` | Smart j/k with jump list support |
| `n` | `<C-d>`/`<C-u>` | Scroll half page (cursor centered) |
| `n` | `G` | Go to bottom (cursor centered) |
| `n` | `n`/`N` | Next/previous search (cursor centered) |
| `n` | `<C-h/j/k/l>` | Navigate between windows |
| `t` | `<C-h/j/k/l>` | Navigate between windows from terminal |

## Buffer Management

| Mode | Keybinding | Description |
|------|------------|-------------|
| `n` | `<C-i>`/`<C-o>` | Previous/next buffer |
| `n` | `<leader>i`/`<leader>o` | Previous/next buffer (fallback) |
| `n` | `<C-q>` | Close current buffer (smart delete) |
| `n` | `<Esc>`/`<C-c>` | Clear search highlights |

## File Explorer & Navigation

| Mode | Keybinding | Description |
|------|------------|-------------|
| `n` | `<leader>d` | Open Oil file manager |
| `n` | `<C-e>` | Toggle NeoTree |
| `n` | `<C-S-e>` | Toggle AI Chat (Claude) |

## Fuzzy Finding (FZF)

| Mode | Keybinding | Description |
|------|------------|-------------|
| `n` | `<C-f>` | Find files |
| `n` | `<leader>ff` | Find files |
| `n` | `<leader>fl` | Live grep search |
| `n` | `<leader>fg` | Grep in current buffer |
| `n` | `<leader>fb` | Find in open buffers |
| `n` | `z=` | Spell suggestions |

## Terminal

| Mode | Keybinding | Description |
|------|------------|-------------|
| `n,t` | `<C-/>` | Toggle terminal |
| `n,t` | `<leader>/` | Toggle terminal (fallback) |

## Window Management

| Mode | Keybinding | Description |
|------|------------|-------------|
| `n,t` | `<C-'>` | Toggle maximize window |
| `n,t` | `<leader>'` | Toggle maximize window (fallback) |
| `n` | `<C-w>'` | Split vertically |
| `n` | `<C-w>5` | Split horizontally |
| `n,t` | `<C-S-Left/Right>` | Resize window horizontally |
| `n,t` | `<C-S-Up/Down>` | Resize window vertically |

## Tab Management

| Mode | Keybinding | Description |
|------|------------|-------------|
| `n` | `<leader>tn` | New tab |
| `n` | `<leader>tk` | Next tab |
| `n` | `<leader>tj` | Previous tab |
| `n` | `<leader>tq` | Close tab |

## Editing & Text Manipulation

| Mode | Keybinding | Description |
|------|------------|-------------|
| `v` | `J`/`K` | Move selection down/up |
| `n` | `J` | Join lines (cursor stays) |
| `v` | `<`/`>` | Indent/unindent (stay in visual) |
| `n,v` | `<leader>s` | Search and replace word under cursor |
| `i` | `jk` | Exit insert mode |
| `i` | `<C-c>` | Exit insert mode |
| `i` | `<C-BS>` | Delete word backward |

## Comments

| Mode | Keybinding | Description |
|------|------------|-------------|
| `n` | `-` | Toggle line comment |
| `v` | `-` | Toggle comment for selection |
| `n` | `_` | Toggle block comment |
| `v` | `_` | Toggle block comment for selection |

## LSP (Language Server)

| Mode | Keybinding | Description |
|------|------------|-------------|
| `n` | `K` | Hover documentation |
| `n,v` | `gd` | Go to definition |
| `n,v` | `gD` | Go to definition (split) |
| `n` | `gi` | Go to implementation |
| `n` | `gr` | Find references |
| `n` | `gR` | Find references (FZF) |
| `n` | `gh` | Go to declaration |
| `n` | `g.` | Quick actions menu |
| `n` | `<F2>` | Rename symbol |
| `n` | `gO` | Document symbols |
| `n` | `gW` | Workspace symbols |
| `n,v` | `<leader>,` | Format code |
| `i` | `<S-l>k` | Signature help |
| `n` | `<leader>th` | Toggle inlay hints |

## Git Integration

| Mode | Keybinding | Description |
|------|------------|-------------|
| `n` | `<leader>gh` | Preview hunk |
| `n` | `<leader>gb` | Git blame |
| `n` | `<leader>gg` | Open LazyGit |
| `n` | `<leader>gr` | Show pull requests |

## Diagnostics & Trouble

| Mode | Keybinding | Description |
|------|------------|-------------|
| `n` | `<leader>xd` | Buffer diagnostics |
| `n` | `<leader>xw` | Workspace diagnostics |
| `n` | `<leader>xq` | Quickfix list |
| `n` | `<leader>xl` | Location list |
| `n` | `]t` | Next trouble item |
| `n` | `[t` | Previous trouble item |

## Refactoring

| Mode | Keybinding | Description |
|------|------------|-------------|
| `x` | `<leader>re` | Extract method |
| `x` | `<leader>rf` | Extract method to file |
| `x` | `<leader>rv` | Extract variable |
| `n,x` | `<leader>ri` | Inline variable |
| `n` | `<leader>rI` | Inline function |
| `n` | `<leader>rb` | Extract block |
| `n` | `<leader>rbf` | Extract block to file |

## Task Runner (Overseer)

| Mode | Keybinding | Description |
|------|------------|-------------|
| `n` | `<leader>tr` | Run task |
| `n` | `<leader>ti` | Task info |
| `n` | `<leader>tt` | Toggle task list |

## Toggles & Utilities

| Mode | Keybinding | Description |
|------|------------|-------------|
| `n` | `<leader>tp` | Toggle spell check |
| `n` | `<leader>tc` | Toggle Copilot |
| `n` | `<leader>tt` | Toggle colorscheme |
| `n` | `<leader>cd` | Change directory to current file |

## Disabled Keys

| Keybinding | Description |
|------------|-------------|
| `<Space>` | Disabled (used as leader) |
| `<F1>` | Disabled |
| `<C-z>` | Disabled |

## Notes

- **Leader key**: `<Space>` is the main leader key
- **Terminal navigation**: All window navigation works from terminal mode
- **Smart movement**: j/k movements add to jump list when using counts
- **Centered navigation**: Search results and scrolling keep cursor centered
- **FZF integration**: Most finding operations use FZF for fast fuzzy searching
- **LSP support**: Full language server integration with hover, definitions, references
- **Git workflow**: Integrated git operations with LazyGit and gitsigns
- **AI assistance**: Claude Code integration for AI-powered development