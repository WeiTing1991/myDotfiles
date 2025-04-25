local M ={}

M = {
  -- formatter
  lua = { "stylua" },
  python = { "ruff" },
  json = { "prettier" },
  cSharp = { "csharpier" },

  -- svelte = { "eslint_d" },
  -- kotlin = { "ktlint" },
  -- terraform = { "tflint" },

  -- linter
  -- jsonc = { "prettier" },
  -- typescript = { "prettier" },
  -- javascript = { "prettier" },
  -- javascriptreact = { "prettier" },
  -- typescriptreact = { "prettier" },
  -- javascript = { "eslint_d" },
  -- typescript = { "eslint_d" },
  -- javascriptreact = { "eslint_d" },
  -- typescriptreact = { "eslint_d" },
  -- { "biome" },
  -- { "prettierd" },

  sh = { "shfmt" },
}

return M

