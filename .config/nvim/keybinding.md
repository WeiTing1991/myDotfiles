# Neovim Keybindings Reference

## Table of Contents
- [Core Keybindings](#core-keybindings)
- [File Navigation & Search (FZF)](#file-navigation--search-fzf)
- [LSP Keybindings](#lsp-keybindings)
- [File Explorer](#file-explorer)
- [Terminal](#terminal)
- [Git](#git)
- [Refactoring](#refactoring)
- [Diagnostics & Troubleshooting](#diagnostics--troubleshooting)
- [Tasks](#tasks)
- [Toggle Options](#toggle-options)
- [FZF Internal Keybindings](#fzf-internal-keybindings)

---

## Core Keybindings

### Navigation
| Key | Mode | Description |
|-----|------|-------------|
| `j` | Normal | Move down (with jump list support for counts > 0) |
| `k` | Normal | Move up (with jump list support for counts > 1) |
| `<C-d>` | Normal | Scroll half page down (cursor centered) |
| `<C-u>` | Normal | Scroll half page up (cursor centered) |
| `G` | Normal | Scroll to bottom (cursor centered) |
| `n` | Normal | Jump to next match (cursor centered) |
| `N` | Normal | Jump to previous match (cursor centered) |

### Buffer Management
| Key | Mode | Description |
|-----|------|-------------|
| `<C-q>` | Normal | Close current buffer and window |
| `<C-i>` | Normal | Previous buffer |
| `<C-o>` | Normal | Next buffer |
| `<leader>i` | Normal | Previous buffer (fallback) |
| `<leader>o` | Normal | Next buffer (fallback) |
| `<leader><leader>` | Normal | Find file in opened buffer |

### Window Management
| Key | Mode | Description |
|-----|------|-------------|
| `<C-h>` | Normal/Terminal | Move to left window |
| `<C-l>` | Normal/Terminal | Move to right window |
| `<C-k>` | Normal/Terminal | Move to up window |
| `<C-j>` | Normal/Terminal | Move to down window |
| `<C-'>` | Normal/Terminal | Toggle maximize window |
| `<leader>'` | Normal/Terminal | Toggle maximize window (fallback) |
| `<C-w>'` | Normal | Split vertically |
| `<C-w>5` | Normal | Split horizontally |
| `<C-S-Left>` | Normal/Terminal | Resize window wider |
| `<C-S-Right>` | Normal/Terminal | Resize window narrower |
| `<C-S-Up>` | Normal/Terminal | Resize window taller |
| `<C-S-Down>` | Normal/Terminal | Resize window shorter |

### Tab Management
| Key | Mode | Description |
|-----|------|-------------|
| `<leader>tk` | Normal | Next tab |
| `<leader>tn` | Normal | New tab |
| `<leader>tq` | Normal | Close tab |
| `<leader>tj` | Normal | Previous tab |

### Editing
| Key | Mode | Description |
|-----|------|-------------|
| `jk` | Insert | Exit insert mode |
| `<C-c>` | Insert | Exit insert mode |
| `<Esc>` | Insert | Exit insert mode |
| `<C-BS>` | Insert | Delete word backward |
| `J` | Visual | Move selection down |
| `K` | Visual | Move selection up |
| `J` | Normal | Join lines (keep cursor position) |
| `<` | Visual | Indent left (keep selection) |
| `>` | Visual | Indent right (keep selection) |
| `-` | Normal | Toggle comment |
| `-` | Visual | Toggle comment |
| `_` | Normal | Toggle comment blockwise |
| `_` | Visual | Toggle comment blockwise |

### Search & Replace
| Key | Mode | Description |
|-----|------|-------------|
| `<Esc>` | Normal | Clear search highlights |
| `<C-c>` | Normal | Clear search highlights |
| `<leader>s` | Normal | Search and replace in quickfix |
| `<leader>S` | Normal/Visual | Simple search and replace current word |

---

## File Navigation & Search (FZF)

| Key | Mode | Description |
|-----|------|-------------|
| `<C-f>` | Normal | Find files |
| `<leader>ff` | Normal | Find files |
| `<leader>fl` | Normal | Live grep |
| `<leader>fg` | Normal | Grep in current buffer |
| `<leader>fb` | Normal | Find file in opened buffer |
| `z=` | Normal | Spell suggestions (FZF) |

---

## LSP Keybindings

### Navigation
| Key | Mode | Description |
|-----|------|-------------|
| `K` | Normal | Hover documentation |
| `gd` | Normal/Visual | Go to definition |
| `gD` | Normal/Visual | Go to definition (split) |
| `gi` | Normal | Go to implementation |
| `gr` | Normal | Find all references |
| `gR` | Normal | Find all references (FZF) |
| `gh` | Normal | Go to header declaration |
| `gO` | Normal | Go to symbol in file (FZF) |
| `go` | Normal | Document symbols (Outline) |
| `gW` | Normal | Go to symbol in workspace (FZF) |

### Code Actions
| Key | Mode | Description |
|-----|------|-------------|
| `g.` | Normal | Quick actions menu (Code action/Spell suggest/AI action) |
| `<F2>` | Normal | Rename symbol |
| `<leader>,` | Normal/Visual | Format code |
| `<S-k>` | Insert | Signature help |

### Toggles
| Key | Mode | Description |
|-----|------|-------------|
| `<leader>th` | Normal | Toggle inlay hints |

---

## File Explorer

| Key | Mode | Description |
|-----|------|-------------|
| `<leader>d` | Normal | Toggle Oil file explorer |
| `<C-e>` | Normal | Toggle Neo-tree |
| `<C-S-e>` | Normal | Toggle AI Chat (Claude) |

---

## Terminal

| Key | Mode | Description |
|-----|------|-------------|
| `<C-/>` | Normal/Terminal | Toggle terminal |
| `<leader>/` | Normal/Terminal | Toggle terminal (fallback) |

---

## Git

| Key | Mode | Description |
|-----|------|-------------|
| `<leader>gh` | Normal | Preview hunk |
| `<leader>gb` | Normal | Git blame |
| `<leader>gg` | Normal | LazyGit |
| `<leader>gr` | Normal | Show PR (GitHub) |

---

## Refactoring

| Key | Mode | Description |
|-----|------|-------------|
| `<leader>re` | Visual | Extract method |
| `<leader>rf` | Visual | Extract method to file |
| `<leader>rv` | Visual | Extract variable |
| `<leader>ri` | Normal/Visual | Inline variable |
| `<leader>rI` | Normal | Inline function |
| `<leader>rb` | Normal | Extract block |
| `<leader>rbf` | Normal | Extract block to file |

---

## Diagnostics & Troubleshooting

| Key | Mode | Description |
|-----|------|-------------|
| `<leader>xd` | Normal | Diagnostics (current buffer) |
| `<leader>xw` | Normal | Diagnostics (workspace) |
| `<leader>xq` | Normal | Quickfix list |
| `<leader>xl` | Normal | Location list |
| `]t` | Normal | Next Trouble item |
| `[t` | Normal | Previous Trouble item |

---

## Tasks

| Key | Mode | Description |
|-----|------|-------------|
| `<leader>tr` | Normal | Run task (Overseer) |

---

## Toggle Options

| Key | Mode | Description |
|-----|------|-------------|
| `<leader>tt` | Normal | Toggle colorscheme |
| `<leader>tp` | Normal | Toggle spell check |
| `<leader>tc` | Normal | Toggle Copilot |

---

## FZF Internal Keybindings

These keybindings work within the FZF picker interface:

### Builtin Actions
| Key | Description |
|-----|-------------|
| `<M-Esc>` | Hide FZF (resume with `:FzfLua resume`) |
| `<F1>` | Toggle help |
| `<F2>` | Toggle fullscreen |
| `<F3>` | Toggle preview wrap |
| `<F4>` | Toggle preview |
| `<F5>` | Rotate preview counter-clockwise |
| `<F6>` | Rotate preview clockwise |
| `<F7>` | Toggle preview treesitter context |
| `<S-Left>` | Reset preview |
| `<S-Down>` | Preview page down |
| `<S-Up>` | Preview page up |
| `<M-S-Down>` | Preview scroll down |
| `<M-S-Up>` | Preview scroll up |

### FZF Actions
| Key | Description |
|-----|-------------|
| `<C-a>` | Toggle all selections |
| `<C-g>` | Jump to first item |
| `<C-G>` | Jump to last item |
| `<C-q>` | Select all and accept |
| `<Enter>` | Edit file or send to quickfix |
| `<C-s>` | Open in horizontal split |
| `<C-v>` | Open in vertical split |
| `<C-t>` | Send to Trouble |
| `<Alt-q>` | Send selection to quickfix |
| `<Alt-Q>` | Send selection to location list |
| `<Alt-i>` | Toggle ignore files |
| `<Alt-h>` | Toggle hidden files |
| `<Alt-f>` | Toggle follow |

---

## Disabled Keys

The following keys are explicitly disabled to avoid conflicts:
- `<Space>` (mapped to leader)
- `<F1>` (disabled default help)
- `<C-z>` (disabled suspend)
