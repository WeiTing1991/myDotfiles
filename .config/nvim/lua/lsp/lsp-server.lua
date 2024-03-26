local M = {}

M = {
  gopls = {
    settings = {
      gopls = {
        completeUnimported = true,
        usePlaceholders = true,
        gofumpt = true,
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
  templ = {
    settings = {
      templ = {
        filetypes = { 'templ' },
      },
    },
  },
  htmx = {
    settings = {
      htmx = {
        filetypes = { 'html' },
      },
    },
  },
  html = {
    settings = {
      html = {
        filetypes = { 'html', 'templ' },
      },
    },
  },
  tailwindcss = {
    settings = {
      tailwindcss = {
        filetypes = { 'templ' },
        init_options = {
          userLanguages = {
            templ = 'html',
          },
        },
      },
    },
  },

  jdtls = {},
  -- lua
  lua_ls = {
    settings = {
      Lua = {
        runtime = { version = 'LuaJIT' },
        diagnostics = {
          globals = { 'vim', 'it', 'describe', 'before_each', 'after_each' },
        },
        workspace = {
          checkThirdParty = false,
          -- Tells lua_ls where to find all the Lua files that you have loaded
          -- for your neovim configuration.
          library = {},
        },
        completion = {
          callSnippet = 'Replace',
        },
        telemetry = {
          enable = false,
        },
      },
    },
  },
  -- spelling
  marksman = {},
  ltex = {
    settings = {
      language = 'en',
    },
  },
}
return M
