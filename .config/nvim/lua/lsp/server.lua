local M = {}
local spell_words = {}
for word in io.open(vim.fn.stdpath "config" .. "/spell/en.utf-8.add", "r"):lines() do
  table.insert(spell_words, word)
end

-- https://github.com/neovim/nvim-lspconfig/blob/master/lua/lspconfig
M = {
  --- lua
  ["lua_ls"] = {
    settings = {
      Lua = {
        completion = { callSnippet = "Replace" },
        format = { enable = false },
        diagnostics = {
          globals = { "vim" },
          disable = { "missing-fields" },
        },
        workspace = {
          checkThirdParty = false,
          library = {
            vim.env.VIMRUNTIME,
            "${3rd}/luv/library",
          },
        },
      },
    },
  },

  ["bashls"] = {
    filetypes = { "bash", "sh", "zsh" },
  },

  --- python
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

  -- md
  ["marksman"] = {
    -- capabilities = {
    --   workspace = {
    --     workspaceFolders = {
    --       -- supported = "false",
    --     },
    --   },
    -- },
  },

  -- json
  ["jsonls"] = {
    filetypes = { "json", "jsonc" },
    settings = {
      json = {
        validate = { enable = true },
        schemas = require("schemastore").json.schemas(),
      },
    },
  },

  -- yaml
  ["yamlls"] = {
    filetypes = { "yaml" },
    settings = {
      yaml = {
        -- Using the schemastore plugin for schemas.
        schemastore = { enable = false, url = "" },
        schemas = require("schemastore").yaml.schemas(),
      },
    },
  },
  -- toml
  ["taplo"] = {
    filetypes = { "toml" },
    settings = {
      taplo = {
        configFile = { enabled = true },
        schema = {
          enabled = true,
          catalogs = { "https://www.schemastore.org/api/json/catalog.json" },
          cache = {
            memoryExpiration = 60,
            diskExpiration = 600,
          },
        },
      },
    },
  },

  -- C#
  ["omnisharp"] = {},

  -- js/ts/css/html
  -- Switched to ts_ls tool
  ["ts_ls"] = {},
  ["cssls"] = {
    cmd = { "css-languageserver", "--stdio" },
    filetypes = { "css", "scss", "less"},
    settings = {
        css = { validate = true },
        scss = { validate = true },
        less = { validate = true },
    },
  },
  ["tailwindcss"] = {},
  ["html"] = {
    filetypes = { "html", "templ" },
  },

  -- docker
  ["dockerls"] = {},
  ["docker_compose_language_service"] = {},

  -- c/c++
  -- ["clangd"] = {
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


  -- HAVE to install go global
  -- go
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
