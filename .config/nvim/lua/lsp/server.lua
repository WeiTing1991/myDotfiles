local M = {}

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

}

return M
