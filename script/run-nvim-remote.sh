#!/usr/bin/zsh

nvim --headless --listen 127.0.0.1:9999 &

# Capture Neovim's Process ID (PID)
NVIM_PID=$!

# Wait for Neovim to initialize (checking TCP port)
echo "Waiting for Neovim to initialize..."
until nc -z 127.0.0.1 9999; do
  sleep 1
done

# Start Goneovim (or Neovide) and connect to the Neovim server
echo "Starting client and connecting to Neovim server..."
goneovim.exe --server localhost:9999
# Neovide.exe --server localhost:9999

# Wait for Neovim process to exit
wait $NVIM_PID

echo "Closing Neovim server..."
kill $NVIM_PID

# Echo when Neovim has been closed and Goneovim should exit
echo "Neovim has closed."
