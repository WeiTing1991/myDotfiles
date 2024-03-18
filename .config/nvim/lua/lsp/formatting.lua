local M = {}

M = {
  lua = { "stylua" },
  markdown = { "prettierd" },
  go = { "gofumpt", "goimports", "golines" },
  java = { "google-java-format" },
  --python = { "isort", "black" },
  -- javascript = { { "prettierd", "prettier" } },
}

return M
