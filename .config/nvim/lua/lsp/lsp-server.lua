-- NOTE: https://github.com/pwntester/nvim-lsp/tree/master?tab=readme-ov-file#bashls
local M = {}

M = {
  bashls = {},

  docker_compose_language_service = {},
  dockerls = {},

  jsonls = {},

  cmake = {
    settings = {
      cmake = {
        filetypes = { "CMakeLists", "cmake" },
      },
    },
  },
  gopls = {
    settings = {
      --check http://www.lazyvim.org/extras/lang/go
      gopls = {
        completeUnimported = true,
        usePlaceholders = true,
        gofumpt = true,
        staticcheck = true,
        semanticTokens = false,
        analyses = {
          unusedparams = true,
          -- shadow = true,
          -- unusedwrite = true,
          -- fieldalignment = true,
        },
      },
    },
    flags = {
      debounce_text_changes = 150,
    },
  },
  templ = {},
  htmx = {
    settings = {
      htmx = {
        filetypes = { "html", "templ" },
      },
    },
  },
  html = {
    settings = {
      html = {
        filetypes = { "html", "templ" },
      },
    },
  },
  tailwindcss = {
    settings = {
      tailwindcss = {
        filetypes = { "html", "templ" },
        init_options = {
          userLanguages = {
            templ = "html",
          },
        },
      },
    },
  },
  clangd = {
    cmd = {
      "clangd",
      "--offset-encoding=utf-16",
    },
    settings = {
      clangd = {
        filetypes = { "c", "cpp", "objc", "objcpp" },
      },
    },
  },
  pyright = {},

  -- jdtls = {},
  -- lua
  lua_ls = {
    settings = {
      Lua = {
        runtime = { version = "LuaJIT" },
        diagnostics = {
          globals = { "vim", "it", "describe", "before_each", "after_each" },
        },
        workspace = {
          checkThirdParty = false,
          -- Tells lua_ls where to find all the Lua files that you have loaded
          -- for your neovim configuration.
          library = {},
        },
        completion = {
          callSnippet = "Replace",
        },
        telemetry = {
          enable = false,
        },
      },
    },
  },
  -- spelling
  marksman = {
    settings = {
      marksman = {
        filetypes = { "markdown" },
      },
    },
  },
  ltex = {
    settings = {
      filetypes = { "markdown" },
      language = "en",
    },
  },
}
return M
