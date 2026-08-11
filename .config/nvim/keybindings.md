# Neovim Keybindings Reference

## Notes

- **Leader key**: `<Space>` is the main leader key
- **Local Leader**: `,`
- **Terminal navigation**: All window navigation works from terminal mode
- **Smart movement**: j/k movements add to jump list when using counts
- **Centered navigation**: Search results and scrolling keep cursor centered
- **FZF integration**: Most finding operations use FZF for fast fuzzy searching

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
| `n` | `<leader>o`/`<leader>i` | Next/previous buffer |
| `n` | `<C-q>` | Close current buffer and window |
| `n` | `<leader><leader>` | Find buffer (FZF) |
| `n` | `<Esc>`/`<C-c>` | Clear search highlights |

## File Explorer & Navigation

| Mode | Keybinding | Description |
|------|------------|-------------|
| `n` | `<leader>d` | Open Oil file manager |
| `n` | `<C-e>` | Toggle NeoTree |

## Fuzzy Finding (FZF)

| Mode | Keybinding | Description |
|------|------------|-------------|
| `n` | `<C-p>` | Find files |
| `n` | `<leader>ff` | Find files |
| `n` | `<leader>fl` | Live grep search |
| `n` | `<leader>fg` | Grep in current buffer |
| `n` | `<leader>fb` | Find in open buffers |
| `n` | `z=` | Spell suggestions |

## Terminal

| Mode | Keybinding | Description |
|------|------------|-------------|
| `n,t` | `` <C-`> `` | Toggle terminal |

## Window Management

| Mode | Keybinding | Description |
|------|------------|-------------|
| `n,t` | `<leader>'` | Toggle maximize window |
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
| `n,v` | `<leader>S` | Search and replace word under cursor |
| `n` | `<leader>s` | Search and replace in quickfix |
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
| `n,v` | `gD` | Go to definition (vsplit) |
| `n` | `gi` | Go to implementation |
| `n` | `gr` | Find references |
| `n` | `gR` | Find references (FZF) |
| `n` | `gh` | Go to declaration |
| `n` | `g.` | Code action |
| `n` | `<F2>` | Rename symbol |
| `n` | `gO` | Document symbols (FZF) |
| `n` | `<leader>co` | Document symbols (Outline) |
| `n` | `gW` | Workspace symbols (FZF) |
| `n,v` | `<leader>,` | Format code |
| `i` | `<C-k>` | Signature help |
| `n` | `<leader>th` | Toggle inlay hints |

## Completion (blink.cmp)

| Mode | Keybinding | Description |
|------|------------|-------------|
| `i` | `<Tab>` | Accept completion |
| `i` | `<A-j>`/`<A-k>` | Next/previous completion item |
| `i` | `<C-y>` | Show/toggle documentation |
| `i` | `<C-space>` | Show snippets |
| `:` | `<Tab>` | Accept cmdline completion |
| `:` | `<A-j>`/`<A-k>` | Next/previous cmdline item |

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

## AI Tools

| Mode | Keybinding | Description |
|------|------------|-------------|
| `n,x` | `<C-a>` | Ask opencode |
| `n,x` | `<C-x>` | Execute opencode action |
| `n,t` | `<C-.>` | Toggle opencode |
| `n,x` | `go` | Add range to opencode |
| `n` | `goo` | Add line to opencode |
| `n` | `+` | Increment (remapped from `<C-a>`) |

## Toggles & Utilities

| Mode | Keybinding | Description |
|------|------------|-------------|
| `n` | `<leader>tt` | Toggle colorscheme |
| `n` | `<leader>tp` | Toggle spell check (fastspell) |
| `n` | `<leader>tc` | Toggle Copilot |

## Dropbar (Breadcrumbs)

| Mode | Keybinding | Description |
|------|------------|-------------|
| `n` | `<Leader>;` | Pick symbols in winbar |
| `n` | `[;` | Go to start of context |
| `n` | `];` | Select next context |

## FZF Internal Keybindings

These work within the FZF picker interface:

### Builtin Actions
| Key | Description |
|-----|-------------|
| `<M-Esc>` | Hide FZF |
| `<F1>` | Toggle help |
| `<F2>` | Toggle fullscreen |
| `<F3>` | Toggle preview wrap |
| `<F4>` | Toggle preview |
| `<S-Down>`/`<S-Up>` | Preview page down/up |
| `alt-j`/`alt-k` | Navigate up/down |

### FZF Actions
| Key | Description |
|-----|-------------|
| `ctrl-a` | Toggle all selections |
| `ctrl-g`/`ctrl-G` | Jump to first/last item |
| `ctrl-q` | Select all and accept |
| `<Enter>` | Edit file or send to quickfix |
| `<C-s>` | Open in horizontal split |
| `<C-v>` | Open in vertical split |
| `<C-t>` | Send to Trouble |
| `<Alt-q>`/`<Alt-Q>` | Send to quickfix/location list |
| `<Alt-i>` | Toggle ignore files |
| `<Alt-h>` | Toggle hidden files |
| `<Alt-f>` | Toggle follow |

## Disabled Keys

| Keybinding | Reason |
|------------|--------|
| `<Space>` | Used as leader |
| `<F1>` | Disabled default help |
| `<C-z>` | Disabled suspend |
