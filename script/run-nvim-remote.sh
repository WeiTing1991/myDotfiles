#!/usr/bin/zsh


# Find an available port dynamically
PORT=$(comm -23 <(seq 1024 65535 | sort) <(ss -tan | awk 'NR>1 {print $4}' | cut -d: -f2 | sort -u) | shuf | head -n 1)


if [ -z "$PORT" ]; then
  echo "Error: Could not determine find the available port."
  exit 1
fi

# Set the Neovim listen address
NVIM_LISTEN_ADDRESS="127.0.0.1:$PORT"
export NVIM_LISTEN_ADDRESS
echo "Using port $PORT for Neovim server."


# Check if Neovim is already listening on the port
nvim --headless --listen "$NVIM_LISTEN_ADDRESS" >/dev/null 2>&1 &
NVIM_PID=$!
echo "neovim started with pid $nvim_pid"
echo "Starting Neovim server at $NVIM_LISTEN_ADDRESS..."

sleep 0.2

# Ensure Neovim is actually running
if ! nc -z 127.0.0.1 "$PORT" 2>/dev/null; then
  echo "Error: Neovim failed to start on $NVIM_LISTEN_ADDRESS."
  exit 1
fi

# Start Goneovim and connect to the Neovim server
echo "Starting Goneovim and connecting to Neovim server..."
goneovim.exe --server "$NVIM_LISTEN_ADDRESS" &
GONEOVIM_PID=$!
echo "Goneovim started with PID $GONEOVIM_PID"

# Wait for Goneovim to exit
wait "$GONEOVIM_PID"
echo "Goneovim has exited."

# Terminate Neovim server
echo "Terminating Neovim server..."
kill -SIGTERM "$NVIM_PID"
echo "Neovim has closed."
