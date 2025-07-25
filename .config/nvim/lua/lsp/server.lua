local M = {}
local spell_words = {}
for word in io.open(vim.fn.stdpath("config") .. "/spell/en.utf-8.add", "r"):lines() do
  table.insert(spell_words, word)
end

-- NOTE: https://github.com/neovim/nvim-lspconfig/tree/master/lua/lspconfig/configs
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
        },
        venvPath = ".",
        venv = ".venv",
      },
    },
  },

  -- c/c++
  ["clangd"] = {
    capabilities = {
      offsetEncoding = { "utf-16" },
    },
    cmd = {
      "clangd",
      "--background-index",
      "--clang-tidy",
      "--header-insertion=iwyu",
      "--completion-style=detailed",
      "--function-arg-placeholders",
      "--fallback-style=llvm",
      --   "--log=verbose",
      --   "--enable-config",
      --   -- "--compile-commands-dir=" .. vim.fn.getcwd() .. "/VCPKG/buildtrees/pkgconf/x64-windows-dbg",
    },
    init_options = {
      usePlaceholders = true,
      completeUnimported = true,
      clangdFileStatus = true,
    },
  },

  ["neocmakelsp"] ={},

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
        schemas = vim.tbl_deep_extend("force", require("schemastore").yaml.schemas(), {
          ["https://json.schemastore.org/github-workflow.json"] = "/.github/workflows/*.{yml,yaml}",
          ["https://json.schemastore.org/github-action.json"] = "action.{yml,yaml}",
        }),
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

  -- CHAGE TO roslyn
  -- C#
  ["roslyn"] = {},
  -- ["omnisharp"] = {
  --   handlers = {
  --     ["textDocument/definition"] = require("omnisharp_extended").handler,
  --   },
  --   cmd = { "omnisharp", "--languageserver", "--hostPID", tostring(vim.fn.getpid()) },
  --   enable_roslyn_analyzers = true,
  --   organize_imports_on_format = true,
  --   enable_import_completion = true,
  --   -- enable_decompilation_support = true,
  --   filetypes = { "cs", "vb", "csproj", "sln", "slnx", "props", "csx", "targets", "tproj", "slngen", "fproj" },
  --   root_dir = function(fname)
  --     return require'lspconfig'.util.root_pattern('*.sln', '*.csproj', 'omnisharp.json')(fname)
  --       or require'lspconfig'.util.path.dirname(fname)
  --   end,
  --   settings = {
  --     FormattingOptions = {
  --       EnableEditorConfigSupport = true,
  --       OrganizeImports = true,
  --     },
  --     -- MsBuild = {
  --     --   LoadProjectsOnDemand = false,
  --     --   EnablePackageAutoRestore = true,
  --     -- },
  --     RoslynExtensionsOptions = {
  --       EnableAnalyzersSupport = false,
  --       EnableImportCompletion = true,
  --       AnalyzeOpenDocumentsOnly = false,
  --     },
  --     Sdk = {
  --       IncludePrereleases = true,
  --     },
  --   },
  -- },

  -- js/ts/css/html
  ["ts_ls"] = {
    settings = {
      typescript = {
        inlayHints = {
          includeInlayParameterNameHints = "all",
          includeInlayParameterNameHintsWhenArgumentMatchesName = false,
          includeInlayFunctionParameterTypeHints = true,
          includeInlayVariableTypeHints = true,
          includeInlayPropertyDeclarationTypeHints = true,
          includeInlayFunctionLikeReturnTypeHints = true,
          includeInlayEnumMemberValueHints = true,
        },
        suggest = {
          includeCompletionsForModuleExports = true,
          completeFunctionCalls = true,
          autoImports = true,
        },
        format = {
          enable = true,
          insertSpaceAfterCommaDelimiter = true,
          insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces = true,
          insertSpaceAfterSemicolonInForStatements = true,
        },
        updateImportsOnFileMove = {
          enabled = "always",
        },
        autoClosingTags = true,
      },
      javascript = {
        inlayHints = {
          includeInlayParameterNameHints = "all",
          includeInlayParameterNameHintsWhenArgumentMatchesName = false,
          includeInlayFunctionParameterTypeHints = true,
          includeInlayVariableTypeHints = true,
          includeInlayPropertyDeclarationTypeHints = true,
          includeInlayFunctionLikeReturnTypeHints = true,
          includeInlayEnumMemberValueHints = true,
        },
        suggest = {
          includeCompletionsForModuleExports = true,
          autoImports = true,
        },
        format = {
          enable = true,
          insertSpaceAfterCommaDelimiter = true,
          insertSpaceAfterSemicolonInForStatements = true,
          insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces = true,
        },
      },
      completions = {
        completeFunctionCalls = true,
      },
    },
  },

  ["cssls"] = {
    -- cmd = { "css-languageserver", "--stdio" },
    filetypes = { "css", "scss", "less" },
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

  -- database
  -- ["sqls"] = {},

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
