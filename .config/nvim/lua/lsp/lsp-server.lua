-- NOTE: https://github.com/pwntester/nvim-lsp/tree/master?tab=readme-ov-file#bashls
local M = {}

M = {
  -- lua
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

  -- python
  pyright = {},
  basedpyright = {
    settings = {
      basedpyright = {
        typeCheckingMode = "off",
        logLevel = "error",
      },
    }
  },
  ruff = {},

  -- c/ c++
  clangd = {
    cmd = { "clangd",
      "--clang-tidy",
      "--log=verbose",
      "--enable-config",
      "--compile-commands-dir=" .. vim.fn.getcwd() .. "/VCPKG/buildtrees/pkgconf/x64-windows-dbg",
    },
    root_dir = function()
      return vim.fn.getcwd()
    end,
    capabilities = {
      offsetEncoding = { "utf-16" },
    },
  }
}


return M
