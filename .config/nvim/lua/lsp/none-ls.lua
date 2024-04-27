local null_ls = require "null-ls"

--local formatter = vim.tbl_values(require('lsp.lsp-formater'))
-- NOTE: https://github.com/nvimtools/none-ls.nvim
-- https://github.com/jose-elias-alvarez/null-ls.nvim/blob/main/doc/BUILTINS.md

local formatting = null_ls.builtins.formatting
local diagnostics = null_ls.builtins.diagnostics
require("null-ls").setup {
  debug = false,
  -- on_init = function(new_client, _)
  --   new_client.offset_encoding = "utf-16"
  -- end,
  sources = {
    -- lua
    -- formating
    formatting.stylua,

    -- linting
    -- diagnostics.selene,

    --go
    formatting.gofumpt,
    formatting.goimports,

    --
    --prettierd
    formatting.prettierd,

    -- java
    -- formating
    formatting.google_java_format,

    -- c/c++
    formatting.clang_format.with {
      filetypes = { "c", "cpp", "objc", "objcpp" },
      args = { "--style=file:" .. vim.fn.stdpath "config" .. "/.clang-format" },
    },

    -- python
    require("none-ls.diagnostics.ruff").with {
      filetypes = { "python" },
    },
    -- require("none-ls.diagnostics.flake8").with{
    --   filetypes = { "python" },
    -- },
    formatting.isort.with {
      filetypes = { "python" },
    },
    formatting.black.with {
      filetypes = { "python" },
      extra_arges = { "--fast" },
    },
  },
}
