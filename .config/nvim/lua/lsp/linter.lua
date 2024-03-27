local M = {}

M = {
  lua = { 'luacheck' },
  markdown = { 'vale' },
  -- NOTE: https://golangci-lint.run/
  go = { 'golangcilint' },
  --java = { "checkstyle" },
  --python = {},
  --javascript = {},
}

return M
