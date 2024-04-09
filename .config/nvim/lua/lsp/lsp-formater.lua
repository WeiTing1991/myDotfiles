local M = {}

M = {
  lua = { "stylua" },
  markdown = { "prettierd" },
  go = { "gofumpt", "goimports", "golines" },
  java = { "google-java-format" },
  templ = { "templ" },
  cpp = { "clang-format" },
  python = { "isort", "black" },
  --html = { 'prettierd' },
  --htmx = { 'prettierd' },
  -- javascript = { { "prettierd", "prettier" } },
}

return M
