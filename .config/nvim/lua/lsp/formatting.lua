local M = {}

M = {
  lua = { 'stylua' },
  markdown = { 'prettierd' },
  go = { 'gofumpt', 'goimports', 'golines' },
  java = { 'google-java-format' },
  html = { 'prettierd' },
  htmx = { 'prettierd' },
  templ = { 'templ' },
  cpp = { 'clang-format' },
  --python = { "isort", "black" },
  -- javascript = { { "prettierd", "prettier" } },
}

return M
