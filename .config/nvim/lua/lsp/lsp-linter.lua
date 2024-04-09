local M = {}

M = {
  lua = { "selene" },
  -- NOTE: https://golangci-lint.run/
  --
  --go = { 'golangcilint' },
  python = { "ruff", "flake8" },

  -- markdown = { 'vale' },

  --java = { "checkstyle" },
  --javascript = {},
}

return M
