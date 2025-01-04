-- lsp server
local M = {}

local spell_words = {}
for word in io.open(vim.fn.stdpath "config" .. "/spell/en.utf-8.add", "r"):lines() do
  table.insert(spell_words, word)
end

M = {
  -- lua
  lua_ls = {
    settings = {
      Lua = {
        diagnostics = {
          globals = { "vim", "it", "describe", "before_each", "after_each" },
          disable = { "missing-fields" },
        },
        completion = {
          callSnippet = "Replace",
        },
      },
    },
  },
  marksman = {
    settings = {
      marksman = {
        filetypes = { "markdown", "markdown.mdx" },
      },
    },
  },

  -- go
  gopls = {
    settings = {
      gopls = {
        analyses = {
          unusedparams = true,
        },
        staticcheck = true,
        gofumpt = true,
        semanticTokens = {
          enable = true, -- Enable semantic tokens
        },
      },
    },
  },

  --ltex
  -- ltex = {
  --   settings = {
  --     ltex = {
  --       enabled = false,
  --       language = "en-US",
  --       dictionary = {
  --         ["en-US"] = spell_words,
  --       },
  --     },
  --   },
  -- },

  -- bashls = {},

  -- python
  ruff = {
    cmd_env = { RUFF_TRACE = "messages" },
    init_options = {
      settings = {
        logLevel = "error",
      },
    },
  },

  pyright = {
    settings = {
      pyright = {
        disableOrganizeImports = true,
      },
      python = {
        analysis = {
          ignore = { "*" },
          -- typeCheckingMode = "basic",
        },
      },
    },
  },
  -- ruff_lsp = {},
  -- basedpyright = {
  --   settings = {
  --     basedpyright = {
  --       typeCheckingMode = "off",
  --       logLevel = "error",
  --     },
  --   },
  -- },
  --

  -- -- c/c++
  clangd = {
    --   cmd = {
    --     "clangd",
    --     "--clang-tidy",
    --     "--log=verbose",
    --     "--enable-config",
    --     -- "--compile-commands-dir=" .. vim.fn.getcwd() .. "/VCPKG/buildtrees/pkgconf/x64-windows-dbg",
    --   },
    --   root_dir = function()
    --     return vim.fn.getcwd()
    --   end,
    --   capabilities = {
    --     offsetEncoding = { "utf-16" },
    --   },
  },

  -- js/ts/css/html
  ts_ls = {
    settings = {
      typescript = {
          suggest = {
            completeFunctionCalls = true,
          },
      }
    }
  },
  -- eslint = {},
  cssls = {},
  tailwindcss = {},
  html = {},

  -- jsonls = {},
}

return M
