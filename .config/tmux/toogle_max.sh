export TMUX_PANE_DIRECTION="bottom"

if [[ "$TMUX_PANE_DIRECTION" == "bottom" ]]; then
  tmux select-pane -U
  tmux resize-pane -Z
elif [[ "$TMUX_PANE_DIRECTION" == "right" ]]; then
  tmux select-pane -L
  tmux resize-pane -Z
elif [[ "$TMUX_PANE_DIRECTION" == "top" ]]; then
  tmux resize-pane -Z
fi


