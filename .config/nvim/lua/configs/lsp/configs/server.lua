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

  -- python
  -- mason install ruff-lsp
  -- ["ruff-lsp"] = {},
  ["ruff"] = {
    -- cmd_env = { RUFF_TRACE = "messages" },
    -- init_options = {
    --   settings = {
    --     logLevel = "error",
    --   },
    -- },
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

  -- js/ts/css/html
  -- Switched to ts_ls tool
  ["ts_ls"] = {},
  ["cssls"] = {},
  ["tailwindcss"] = {},
  ["html"] = {},
  ["bashls"] = {},

  ["marksman"] = {
    capabilities = {
      workspace = {
        workspaceFolders = {
          supported = false,
        },
      },
    },
  },

  ["yamlls"] = {},
  ["taplo"] = {},
  ["dockerls"] = {},
  ["docker_compose_language_service"] = {},

  ["jsonls"] = {
    settings = {
      json = {
        -- schemas = require("schemastore").json.schemas(),
        validate = { enable = true },
      },
    },
  },

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

}

return M
