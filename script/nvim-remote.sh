#!/bin/bash

#!/usr/bin/zsh

# Default port (can be overridden with an argument)
PORT=${1:-10000}

is_port_in_use() {
  nc -z 127.0.0.1 $1 >/dev/null 2>&1
}

check_port() {
  local port=$1
  if ss -tuln | grep -q ":$port "; then
    return 0 # Port is in use (true)
  else
    return 1 # Port is available (false)
  fi
}

find_available_port() {
  local port=$1
  while check_port "$port"; do
    ((port++)) # Increment port number
  done
  echo "$port" # Return the first available port
}

start_nvim_server() {
  if check_port $1; then
    PORT=$(find_available_port $1)
    echo "Using port $PORT"
  fi
  nvim --headless --listen 127.0.0.1:$PORT &
  NVIM_PID=$!
  sleep 0.5

  if [[ "$(uname)" == "Darwin" ]]; then
    /Applications/FVim.app/Contents/MacOS/FVim --server 127.0.0.0.1:$PORT
  elif [[ "$(uname)" == "Linux" ]]; then
    /mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe -NoProfile -Command "Start-Process 'C:\Users\weitingchen\FVim\fvim.exe' '--server','127.0.0.1:$PORT' -NoNewWindow" &
  fi
  echo "Starting Neovim and connecting to Neovim server on port $PORT..."

  # goneovim.exe --server 127.0.0.1:$PORT &
  # neovide.exe --server 127.0.0.1:$PORT &
}

# Function to handle termination
cleanup() {
  echo "Terminating Neovim server..."
  kill "$NVIM_PID" 2>/dev/null
  sleep 0.5
  echo "Neovim has closed."
}

# Trap signals to ensure cleanup
trap cleanup EXIT

# Start nvim server and goneovim client
start_nvim_server "$PORT"
echo "Neovim server started on port $PORT and fvim launched"

# Wait for Neovim to exit
wait "$NVIM_PID"

# close cleanup trap
trap cleanup EXIT
