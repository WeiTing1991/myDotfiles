local M = {}

local spell_words = {}
for word in io.open(vim.fn.stdpath "config" .. "/spell/en.utf-8.add", "r"):lines() do
  table.insert(spell_words, word)
end

M = {
  -- lua
  ["lua_ls"] = {
    settings = {
      Lua = {
        completion = {
          callSnippet = "Replace",
        },
        diagnostics = {
          globals = { "vim", "it", "describe", "before_each", "after_each" },
          -- disable = { "missing-fields" },
        },
      },
      workspace = {
        checkThirdParty = false,
        library = {
          vim.env.VIMRUNTIME,
        },
      },
    },
  },

  -- -- HAVE to install go global
  -- -- go
  -- gopls = {
  --   settings = {
  --     gopls = {
  --       analyses = {
  --         unusedparams = true,
  --       },
  --       staticcheck = true,
  --       gofumpt = true,
  --       semanticTokens = {
  --         enable = true, -- Enable semantic tokens
  --       },
  --     },
  --   },
  -- },
  --
  --ltex
  ["ltex"] = {
    settings = {
      ltex = {
        enabled = false,
        language = "en-US",
        dictionary = {
          ["en-US"] = spell_words,
        },
      },
    },
  },
  --
  -- python
  -- mason install ruff-lsp
  -- ["ruff-lsp"] = {},
  ["ruff"] = {
    --   cmd_env = { RUFF_TRACE = "messages" },
    --   init_options = {
    --     settings = {
    --       logLevel = "error",
    --     },
    --   },
    -- },
    --
  },

  ["pyright"] = {
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
  -- basedpyright = {
  --   settings = {
  --     basedpyright = {
  --       typeCheckingMode = "off",
  --       logLevel = "error",
  --     },
  --   },
  -- },

  -- c/c++
  -- clangd = {
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
  -- },

  -- js/ts/css/html
  ["ts_ls"] = {},
  -- ts_ls = {
  --   settings = {
  --     typescript = {
  --       inlayHints = {
  --         includeInlayParameterNameHints = "literal",
  --         includeInlayParameterNameHintsWhenArgumentMatchesName = false,
  --         includeInlayFunctionParameterTypeHints = true,
  --         includeInlayVariableTypeHints = false,
  --         includeInlayPropertyDeclarationTypeHints = true,
  --         includeInlayFunctionLikeReturnTypeHints = true,
  --         includeInlayEnumMemberValueHints = true,
  --       },
  --       suggest = {
  --         completeFunctionCalls = true,
  --       },
  --     },
  --     javascript = {
  --       inlayHints = {
  --         includeInlayParameterNameHints = "all",
  --         includeInlayParameterNameHintsWhenArgumentMatchesName = false,
  --         includeInlayFunctionParameterTypeHints = true,
  --         includeInlayVariableTypeHints = true,
  --         includeInlayPropertyDeclarationTypeHints = true,
  --         includeInlayFunctionLikeReturnTypeHints = true,
  --         includeInlayEnumMemberValueHints = true,
  --       },
  --     },
  --   },
  -- },

  ["cssls"] = {},
  ["tailwindcss"] = {},
  ["html"] = {},
  ["bashls"] = {},
  ["marksman"] = {},
  ["yamlls"] = {},

  ["jsonls"] = {
    settings = {
      json = {
        -- schemas = require("schemastore").json.schemas(),
        validate = { enable = true },
      },
    },
  },

}

return M
