local luacheck = require("efmls-configs.linters.luacheck")
--
-- https://github.com/mattn/efm-langserver
-- Note https://github.com/mfussenegger/nvim-lint
local lspconfig = require("lspconfig")
lspconfig.efm.setup({
  filetypes = {
    "lua",
  },
  init_options = {
    documentFormatting = true,
    documentRangeFormatting = true,
    hover = true,
    documentSymbol = true,
    codeAction = true,
    completion = true,
  },
  settings = {
    languages = {
      lua = { luacheck },
    },
  },
})
