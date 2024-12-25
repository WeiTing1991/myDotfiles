# notes
# General keybinding for prefix
unbind C-b
set-option -g prefix C-a
bind-key C-a send-prefix

# Pane Management
bind '"' split-window -h           # Split the pane horizontally
bind '%' split-window -v           # Split the pane vertically
bind o select-pane -t :.+         # Switch to the next pane
bind x kill-pane                   # Close the current pane
bind <Left> resize-pane -L         # Resize the current pane to the left
bind <Right> resize-pane -R        # Resize the current pane to the right
bind <Up> resize-pane -U           # Resize the current pane upwards
bind <Down> resize-pane -D         # Resize the current pane downwards
bind z resize-pane -Z              # Maximize the current pane

# Window Management
bind c new-window                  # Create a new window
bind n next-window                 # Switch to the next window
bind p previous-window             # Switch to the previous window
bind w choose-window               # Show a list of windows
bind & kill-window                 # Close the current window

# Session Management
bind d detach                      # Detach the session
bind s list-sessions               # List sessions
bind $ rename-session             # Rename the session
bind : command-prompt             # Open the command prompt

# Copy Mode
bind [ copy-mode                   # Enter copy mode
bind q send-keys q                # Exit copy mode
bind Space copy-mode -b           # Start text selection in copy mode
bind Enter copy-selection-and-exit # Copy the selected text

# Scrolling in Copy Mode
bind -n Up copy-mode -u           # Scroll up
bind -n Down copy-mode -d         # Scroll down
bind -n C-u copy-mode -u          # Scroll up by half a page
bind -n C-d copy-mode -d          # Scroll down by half a page

# s  list sessions
# $  name session
# Windows (tabs)
# c  create window
# w  list windows
# n  next window
# p  previous window
# f  find window
# ,  name window
# &  kill window
# Panes (splits)
# %  vertical split
# "  horizontal split
#
# o  swap panes
# q  show pane numbers
# x  kill pane
# +  break pane into window (e.g. to select text by mouse to copy)
# -  restore pane from window
# ⍽  space - toggle between layouts
# <prefix> q (Show pane numbers, when the numbers show up type the key to goto that pane)
# <prefix> { (Move the current pane left)
# <prefix> } (Move the current pane right)
# <prefix> z toggle pane zoom
