-- formate and linter
local M = {}

M = {
  -- formatter
  lua = { "stylua" },
  python = { "ruff" },

  json = { "prettier" },
    -- { "prettierd" },
  github = { "actionlint" },
  sh = { "shfmt" },
  c = { "clang-format" },
  cpp = { "clang-format" },
  cmake = { "cmakelint" },

  -- cSharp = { "csharpier" },
  -- typescript = { "biome" },


  -- linter
  -- javascript = { "eslint_d" },
  -- typescript = { "eslint_d" },
  -- javascriptreact = { "eslint_d" },
  -- typescriptreact = { "eslint_d" },
}

return M
