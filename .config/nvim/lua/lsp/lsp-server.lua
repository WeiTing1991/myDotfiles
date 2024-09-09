-- NOTE: https://github.com/pwntester/nvim-lsp/tree/master?tab=readme-ov-file#bashls
local M = {}

M = {
  lua_ls = {
    settings = {
      Lua = {
        diagnostics = {
          globals = { "vim", "it", "describe", "before_each", "after_each" },
          disable = { 'missing-fields' },
        },
        completion = {
          callSnippet = "Replace",
        },
      },
    },
  },

  pyright = {}

}

return M
