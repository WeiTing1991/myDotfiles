local null_ls = require "null-ls"

-- NOTE: https://github.com/nvimtools/none-ls.nvim
-- https://github.com/jose-elias-alvarez/null-ls.nvim/blob/main/doc/BUILTINS.md
local formatting = null_ls.builtins.formatting

require("null-ls").setup {
  debug = false,
  -- on_init = function(new_client, _)
  --   new_client.offset_encoding = "utf-16"
  -- end,
  sources = {
    -- lua
    -- formating
    formatting.stylua,

    -- python
    -- formating
    require("none-ls.diagnostics.ruff").with {
      filetypes = { "python" },
    },

    formatting.isort.with {
      filetypes = { "python" },
    },

    formatting.black.with {
      filetypes = { "python" },
      extra_arges = { "--fast" },
    },
  },

}
