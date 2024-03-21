local M = {}

M = {
  lua = { 'luacheck' },
  markdown = { 'vale', 'markdownlint' },
  -- NOTE: https://golangci-lint.run/
  go = { 'golangcilint' },
  --java = { "checkstyle" },
  --python = {},
  --javascript = {},
}

return M
