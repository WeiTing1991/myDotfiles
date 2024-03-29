local null_ls = require("null-ls")

--local formatter = vim.tbl_values(require('lsp.lsp-formater'))
-- NOTE: https://github.com/nvimtools/none-ls.nvim
-- https://github.com/jose-elias-alvarez/null-ls.nvim/blob/main/doc/BUILTINS.md

local formatting = null_ls.builtins.formatting
local diagnostics = null_ls.builtins.diagnostics
require("null-ls").setup({
  debug = false,
  sources = {
    -- lua
    -- formating
    formatting.stylua,
    -- linting
    -- diagnostics.selene,

    -- java
    -- formating
    formatting.google_java_format,


    -- C/C++
    formatting.clang_format.with({
      args = { '--style=file:' .. vim.fn.stdpath('config') .. '/.clang-format' },
    })
  },
})