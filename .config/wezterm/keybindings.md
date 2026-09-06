# WezTerm Keybindings

Reference for [`wezterm.lua`](wezterm.lua). `disable_default_key_bindings = true` is set,
so **only** the bindings listed here exist — WezTerm's built-in defaults are all off.

**Leader:** `Ctrl+X` (500 ms timeout). Press it, release, then the next key.
To send a literal `Ctrl+X` to the shell, press `Ctrl+X` then `Ctrl+X`.

Modifier names: `Cmd` = macOS only, `Ctrl` works everywhere.

---

## Search / scrollback

Scrollback is 10000 lines per pane; search only sees what's still in scrollback.

### Open the find bar

| Key | Platform |
| --- | --- |
| `Cmd+F` | macOS |
| `Ctrl+Shift+F` | macOS, Linux, Windows |

Prefills the search box with the current selection if there is one.

### Inside the find bar (`search_mode`)

| Key | Action |
| --- | --- |
| `Enter` | Next match (down) |
| `Shift+Enter` | Previous match (up) |
| `↓` / `Ctrl+N` | Next match |
| `↑` / `Ctrl+P` | Previous match |
| `PageDown` / `PageUp` | Next / previous match, a page at a time |
| `Ctrl+R` | Cycle case-sensitive → case-insensitive → regex |
| `Ctrl+U` | Clear the pattern |
| `Ctrl+Shift+C` / `Cmd+C` | Copy the highlighted match |
| `Esc` | Close, keeping the match selected |

> WezTerm's own default binds `Enter` to *previous* match. This config flips it to
> match IDE and browser behaviour.

---

## Copy mode

Enter with `Ctrl+X` `c`, or by pressing `Esc` out of the find bar.

| Key | Action |
| --- | --- |
| `h` `j` `k` `l` | Move cursor left / down / up / right |
| `Ctrl+D` / `Ctrl+U` | Page down / up |
| `g` / `G` | Top / bottom of scrollback |
| `n` / `N` | Next / previous search match |
| `/` | Reopen the find bar with the current pattern |
| `v` | Start a cell selection |
| `Ctrl+V` | Start a block (rectangular) selection |
| `y` | Copy selection and close |
| `Ctrl+Shift+C` / `Cmd+Shift+C` | Copy to clipboard |
| `Ctrl+Shift+V` / `Cmd+Shift+V` | Paste from clipboard |
| `Esc` | Close copy mode |

---

## Tabs

| Key | Action |
| --- | --- |
| `Ctrl+X` `j` | Next tab |
| `Ctrl+X` `k` | Previous tab |
| `Ctrl+X` `t` | Fuzzy tab switcher (labelled by directory) |
| `Ctrl+X` `<` or `Ctrl+X` `Shift+,` | Move tab left |
| `Ctrl+X` `>` or `Ctrl+X` `Shift+.` | Move tab right |
| `Ctrl+Shift+T` | New tab |
| `Ctrl+Shift+W` | Close tab (no confirmation) |
| `Ctrl+Shift+R` | Rename tab |

Tab titles default to the current directory name; a zoomed pane shows `[Z]`.
Tab and split indices are zero-based.

---

## Panes

| Key | Action |
| --- | --- |
| `Ctrl+Shift+'` | Split vertically |
| `Ctrl+5` | Split horizontally |
| `Ctrl+Shift+H` | Focus pane left |
| `Ctrl+Shift+J` | Focus pane down |
| `Ctrl+Shift+K` | Focus pane up |
| `Ctrl+Shift+L` | Focus pane right |
| `Ctrl+Shift+M` | Toggle pane zoom |
| `Ctrl+X` `p` | List panes — labels each pane, press a label to jump to it |
| `Ctrl+X` `Shift+P` | List panes, then swap the chosen one with the current pane |
| `Ctrl+X` `r` | Enter resize mode (see below) |

The tab bar shows the pane count for the current tab as `[n]` when a tab has more
than one pane (`[Z]` marks a zoomed pane).

### Resize mode

`Ctrl+X` `r` enters a mode that stays active, so you can press the keys repeatedly
without re-pressing the leader. It exits automatically after 1 second of inactivity.

| Key | Action |
| --- | --- |
| `h` `j` `k` `l` | Resize by 3 cells left / down / up / right |
| `Shift+H` `Shift+J` `Shift+K` `Shift+L` | Resize by 1 cell (fine adjustment) |
| `Esc` or `Enter` | Leave resize mode |

---

## Windows, font, clipboard

| Key | Action |
| --- | --- |
| `Ctrl+Shift+N` | New window |
| `Ctrl+=` | Increase font size |
| `Ctrl+-` | Decrease font size |
| `Ctrl+0` | Reset font size |
| `Ctrl+Shift+C` | Copy |
| `Ctrl+Shift+V` | Paste |

Window size stays fixed when the font size changes.

---

## Mouse

| Action | Result |
| --- | --- |
| `Cmd`+click (macOS) / `Ctrl`+click | Open link under cursor |
| `Ctrl`+double-click | Open the pane's working directory in Finder / Explorer |
| Double right-click | Copy selection to clipboard |

---

## Jump to file from output

See [`dispatcher.lua`](dispatcher.lua). Compiler, linter, and grep output is turned into
clickable links, so `Cmd`/`Ctrl`+click on a location opens it in a running Neovim at the
right line and column, then focuses that pane.

Recognised patterns:

- `file.ext:12:5` and `file.ext(12,5)` for `cpp cc cxx hpp h c cs rs py lua`
- `File "path.py", line 12` (Python tracebacks)

Neovim must be listening on `<git-root>/.nvim/socket_dispatcher/nvim.sock` and define a
`Jump(file, line, col)` Lua function.

---

## Output too long to search?

Scrollback search only covers the last 10000 lines. For a bigger build, write it to a
file first:

```bash
# macOS / Linux
<build command> 2>&1 | tee build.log
rg 'error' build.log
```

```powershell
# Windows
<build command> 2>&1 | Tee-Object build.log
Select-String 'error' build.log
```
