local M = {}

local spell_words = {}
for word in io.open(vim.fn.stdpath("config") .. "/spell/en.utf-8.add", "r"):lines() do
  table.insert(spell_words, word)
end

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

  -- shell
  ["bashls"] = {
    filetypes = { "bash", "sh", "zsh" },
  },

  -- markdown
  ["marksman"] = {},

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
    filetypes = { "yaml", "yml" },
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

  -- docker
  ["dockerls"] = {},
  ["docker_compose_language_service"] = {},

  -- python
  -- ["pyright"] = {},
  ["basedpyright"] = {
    settings = {
      basedpyright = {
        analysis = {
          autoSearchPaths = true,
          diagnosticMode = "workspace",
          useLibraryCodeForTypes = true,
          typeCheckingMode = "standard",
          extraPaths = {
            "./src",
            "./lib",
            "./backend",
            "./venv",
            ".env",
            "C:\\Users\\weichen34\\project\\kumiki\\src",
          },
        },
        diagnosticSeverityOverrides = {
          -- reportMissingImports = "information",
          reportMissingModuleSource = "none",
          reportMissingTypeStubs = "none",
          -- reportImportCycles = "none",
          -- reportImportNotFound = "none",
          -- reportOptionalSubscript = "none",
          -- reportOptionalMemberAccess = "none",
          -- reportMissingTypeStubs = false,
          -- reportAttributeAccessIssue = false,
          -- reportUnknownMemberType = false,
          -- reportUndefinedVariable = false,
          -- reportGeneralTypeIssues = false,
        },
      },
    },
  },

  ["ruff"] = {
    on_attach = function(client, bufnr)
      -- Disable hover in ruff to avoid conflicts with pyright
      client.server_capabilities.hoverProvider = false
    end,
  },

  -- c/c++
  ["clangd"] = {
    -- root_dir = require('lspconfig').util.root_pattern(
    --     "compile_commands.json",
    --     ".git",
    --     "CMakeLists.txt"
    -- ),
    root_markers = {
      "compile_commands.json",
      "compile_flags.txt",
      "configure.ac", -- AutoTools
      "Makefile",
      "configure.ac",
      "configure.in",
      "config.h.in",
      "meson.build",
      "meson_options.txt",
      "build.ninja",
      ".git",
      "CMakeLists.txt",
    },
    capabilities = {
      offsetEncoding = { "utf-16" },
    },
    cmd = {
      "clangd",
      "--background-index",
      -- "--clang-tidy",
      "--header-insertion=iwyu",
      "--completion-style=detailed",
      "--function-arg-placeholders",
      "--fallback-style=llvm",
    },
    init_options = {
      usePlaceholders = true,
      completeUnimported = true,
      clangdFileStatus = true,
      semanticHighlighting = true,
    },
  },

  -- cmake
  ["neocmakelsp"] = {
    cmd = { "neocmakelsp", "--stdio" },
    filetypes = { "cmake" },
    single_file_support = true,
    init_options = {
      scan_cmake_in_package = false,
      semantic_token = false,
    },
    settings = {
      neocmakelsp = {
        lint = {
          enable = true,
          lineLength = 120,
        },
        format = {
          enable = true,
        },
        scan_cmake_in_package = false,
      },
    },
  },

  -- C#
  ["roslyn"] = {
    on_attach = function()
      print("Roslyn LSP attached")
    end,
    settings = {
      ["csharp|inlay_hints"] = {
        csharp_enable_inlay_hints_for_implicit_object_creation = true,
        csharp_enable_inlay_hints_for_implicit_variable_types = true,
      },
      ["csharp|code_lens"] = {
        dotnet_enable_references_code_lens = true,
      },
      ["csharp|formating"] = {
        dotnet_organize_imports_on_format = true,
      },
    },
  },
  -- xml
  ["lemminx"] = {
    cmd = { "lemminx" },
    -- cmd = { "java", "-jar", "/path/to/org.eclipse.lemminx-uber.jar" },
    settings = {
      xml = {
        validate = false, -- disable all schema validation
        format = { enabled = true },
        trace = { server = "off" },
        logs = { client = false },
      },
    },
    filetypes = { "xml", "xsd", "svg", "csproj" }, -- make sure .csproj files are included
  },

  --TODO:
  -- -- js/ts/css/html
  -- -- ["ts_ls"] = {
  -- --   settings = {
  -- --     typescript = {
  -- --       inlayHints = {
  -- --         includeInlayParameterNameHints = "all",
  -- --         includeInlayParameterNameHintsWhenArgumentMatchesName = false,
  -- --         includeInlayFunctionParameterTypeHints = true,
  -- --         includeInlayVariableTypeHints = true,
  -- --         includeInlayPropertyDeclarationTypeHints = true,
  -- --         includeInlayFunctionLikeReturnTypeHints = true,
  -- --         includeInlayEnumMemberValueHints = true,
  -- --       },
  -- --       suggest = {
  -- --         includeCompletionsForModuleExports = true,
  -- --         completeFunctionCalls = true,
  -- --         autoImports = true,
  -- --       },
  -- --       format = {
  -- --         enable = true,
  -- --         insertSpaceAfterCommaDelimiter = true,
  -- --         insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces = true,
  -- --         insertSpaceAfterSemicolonInForStatements = true,
  -- --       },
  -- --       updateImportsOnFileMove = {
  -- --         enabled = "always",
  -- --       },
  -- --       autoClosingTags = true,
  -- --     },
  -- --     javascript = {
  -- --       inlayHints = {
  -- --         includeInlayParameterNameHints = "all",
  -- --         includeInlayParameterNameHintsWhenArgumentMatchesName = false,
  -- --         includeInlayFunctionParameterTypeHints = true,
  -- --         includeInlayVariableTypeHints = true,
  -- --         includeInlayPropertyDeclarationTypeHints = true,
  -- --         includeInlayFunctionLikeReturnTypeHints = true,
  -- --         includeInlayEnumMemberValueHints = true,
  -- --       },
  -- --       suggest = {
  -- --         includeCompletionsForModuleExports = true,
  -- --         autoImports = true,
  -- --       },
  -- --       format = {
  -- --         enable = true,
  -- --         insertSpaceAfterCommaDelimiter = true,
  -- --         insertSpaceAfterSemicolonInForStatements = true,
  -- --         insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces = true,
  -- --       },
  -- --     },
  -- --     completions = {
  -- --       completeFunctionCalls = true,
  -- --     },
  -- --   },
  -- -- },
  -- ["vtsls"] = {
  --   filetypes = {
  --     "javascript",
  --     "javascriptreact",
  --     "javascript.jsx",
  --     "typescript",
  --     "typescriptreact",
  --     "typescript.tsx",
  --   },
  --   settings = {
  --     complete_function_calls = true, -- LazyVim has this at root level
  --     vtsls = {
  --       enableMoveToFileCodeAction = true,
  --       autoUseWorkspaceTsdk = true,
  --       experimental = {
  --         maxInlayHintLength = 30,
  --         completion = {
  --           enableServerSideFuzzyMatch = true,
  --         },
  --       },
  --     },
  --     typescript = {
  --       -- Inlay hints (converted from your ts_ls settings)
  --       inlayHints = {
  --         parameterNames = { enabled = "all" }, -- includeInlayParameterNameHints
  --         parameterTypes = { enabled = true }, -- includeInlayFunctionParameterTypeHints
  --         variableTypes = { enabled = true }, -- includeInlayVariableTypeHints
  --         propertyDeclarationTypes = { enabled = true }, -- includeInlayPropertyDeclarationTypeHints
  --         functionLikeReturnTypes = { enabled = true }, -- includeInlayFunctionLikeReturnTypeHints
  --         enumMemberValues = { enabled = true }, -- includeInlayEnumMemberValueHints
  --       },
  --
  --       -- Suggestions (your settings)
  --       suggest = {
  --         includeCompletionsForModuleExports = true,
  --         completeFunctionCalls = true,
  --         autoImports = true,
  --         includeCompletionsForImportStatements = true,
  --         includeCompletionsWithSnippetText = true,
  --         includeAutomaticOptionalChainCompletions = true,
  --       },
  --
  --       -- Format settings (your settings)
  --       format = {
  --         enable = true,
  --         insertSpaceAfterCommaDelimiter = true,
  --         insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces = true,
  --         insertSpaceAfterSemicolonInForStatements = true,
  --         indentSize = 2,
  --         convertTabsToSpaces = true,
  --         tabSize = 2,
  --       },
  --
  --       -- File move settings (your settings)
  --       updateImportsOnFileMove = { enabled = "always" },
  --
  --       -- Auto closing tags (your settings)
  --       autoClosingTags = true,
  --
  --       -- Additional vtsls optimizations
  --       preferences = {
  --         importModuleSpecifier = "relative",
  --         importModuleSpecifierEnding = "minimal",
  --         includePackageJsonAutoImports = "auto",
  --         providePrefixAndSuffixTextForRename = true,
  --         allowRenameOfImportPath = true,
  --         allowTextChangesInNewFiles = true,
  --       },
  --
  --       -- Workspace settings
  --       workspaceSymbols = {
  --         scope = "allOpenProjects",
  --       },
  --     },
  --
  --     javascript = {
  --       -- Inlay hints (your settings)
  --       inlayHints = {
  --         parameterNames = { enabled = "all" }, -- includeInlayParameterNameHints
  --         parameterTypes = { enabled = true }, -- includeInlayFunctionParameterTypeHints
  --         variableTypes = { enabled = true }, -- includeInlayVariableTypeHints
  --         propertyDeclarationTypes = { enabled = true }, -- includeInlayPropertyDeclarationTypeHints
  --         functionLikeReturnTypes = { enabled = true }, -- includeInlayFunctionLikeReturnTypeHints
  --         enumMemberValues = { enabled = true }, -- includeInlayEnumMemberValueHints
  --       },
  --
  --       -- Suggestions (your settings)
  --       suggest = {
  --         includeCompletionsForModuleExports = true,
  --         autoImports = true,
  --         completeFunctionCalls = true,
  --         includeCompletionsForImportStatements = true,
  --         includeCompletionsWithSnippetText = true,
  --         includeAutomaticOptionalChainCompletions = true,
  --       },
  --
  --       -- Format settings (your settings)
  --       format = {
  --         enable = true,
  --         insertSpaceAfterCommaDelimiter = true,
  --         insertSpaceAfterSemicolonInForStatements = true,
  --         insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces = true,
  --         indentSize = 2,
  --         convertTabsToSpaces = true,
  --         tabSize = 2,
  --       },
  --
  --       -- File move settings
  --       updateImportsOnFileMove = { enabled = "always" },
  --
  --       -- Auto closing tags
  --       autoClosingTags = true,
  --
  --       -- Additional settings
  --       preferences = {
  --         importModuleSpecifier = "relative",
  --         importModuleSpecifierEnding = "minimal",
  --         includePackageJsonAutoImports = "auto",
  --       },
  --     },
  --
  --     -- Completions (your settings)
  --     completions = {
  --       completeFunctionCalls = true,
  --     },
  --   },
  -- },
  -- ["cssls"] = {
  --   -- cmd = { "css-languageserver", "--stdio" },
  --   filetypes = { "css", "scss", "less" },
  --   settings = {
  --     css = { validate = true },
  --     scss = { validate = true },
  --     less = { validate = true },
  --   },
  -- },
  -- ["tailwindcss"] = {},
  -- ["html"] = {
  --   filetypes = { "html", "templ" },
  -- },
  --
  --
  -- -- database
  -- -- ["sqls"] = {},
  --
  -- -- HAVE to install go global
  -- -- go
  -- -- gopls = {
  -- --   settings = {
  -- --     gopls = {
  -- --       analyses = {
  -- --         unusedparams = true,
  -- --       },
  -- --       staticcheck = true,
  -- --       gofumpt = true,
  -- --       semanticTokens = {
  -- --         enable = true, -- Enable semantic tokens
  -- --       },
  -- --     },
  -- --   },
  -- -- },
}

return M
