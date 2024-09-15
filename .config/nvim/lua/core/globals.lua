local globals = {
  mapleader = " ",
  prev_buffer = nil,
  next_buffer = nil,
  have_nerd_font = true,
}

for k, v in pairs(globals) do
  vim.g[k] = v
end
