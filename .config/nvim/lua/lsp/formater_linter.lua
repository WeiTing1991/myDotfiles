local M = {}

M = {
  -- formatter
  lua = { "stylua" },
  python = { "ruff" },
  -- { "prettierd" },
  json = { "prettier" },
  github = { "actionlint" },
  typescript = { "biome" },

  sh = { "shfmt" },
  cSharp = { "csharpier" },

  c = { "clang-format" },
  cpp = { "clang-format" },

  -- svelte = { "eslint_d" },
  -- kotlin = { "ktlint" },
  -- terraform = { "tflint" },

  -- linter
  -- javascript = { "eslint_d" },
  -- typescript = { "eslint_d" },
  -- javascriptreact = { "eslint_d" },
  -- typescriptreact = { "eslint_d" },
}

return M
