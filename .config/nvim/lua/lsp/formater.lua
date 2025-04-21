local M = {}

M = {
  lua = { "stylua" },
  python = { "ruff" },

  json = { "prettier" },
  jsonc = { "prettier" },
  typescript = { "prettier" },
  javascript = { "prettier" },
  javascriptreact = { "prettier" },
  typescriptreact = { "prettier" },
  -- { "biome" },
  -- { "prettierd" },

  sh = { "shfmt" },
}

return M
