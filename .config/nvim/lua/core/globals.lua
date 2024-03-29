local globals = {
  mapleader = " ",
  prev_buffer = nil,
  next_buffer = nil,
  netrw_browse_split = 0,
  --netrw_banner  = 0,
  --netrw_altv = 1,
  --netrw_liststyle = 3,
  netrw_winsize = 25,
}

for k, v in pairs(globals) do
  vim.g[k] = v
end

